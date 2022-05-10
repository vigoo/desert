package io.github.vigoo.desert

import io.github.vigoo.desert.BinaryDeserializer.{Deser, DeserializationEnv}
import io.github.vigoo.desert.BinaryDeserializerOps.{failDeserializerWith, finishDeserializerWith, getDeserializerState, getInput, getInputTypeRegistry, read, readByte, readBytes, readVarInt, setDeserializerState}
import io.github.vigoo.desert.BinarySerializer.{Ser, SerializationEnv}
import io.github.vigoo.desert.BinarySerializerOps.{failSerializerWith, finishSerializer, getOutput, getOutputTypeRegistry, getSerializerState, setSerializerState, write, writeByte, writeBytes, writeVarInt}
import io.github.vigoo.desert.GenericBinaryCodec.{ChunkedInput, ChunkedOutput, ChunkedSerState, FieldPosition, SerializedEvolutionStep}
import io.github.vigoo.desert.codecs.DeduplicatedString
import zio.prelude.fx.ZPure

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

class AdtCodec[T](evolutionSteps: Vector[Evolution],
                  constructorMap: Vector[String],
                  typeDescription: String) {
  private val version: Byte = (evolutionSteps.size - 1).toByte
  private val fieldGenerations: Map[String, Byte] =
    evolutionSteps
      .zipWithIndex
      .collect {
        case (FieldAdded(name, _), idx) => (name, idx)
      }
      .map { case (name, idx) => (name, idx.toByte) }
      .toMap
  private val fieldDefaults: Map[String, Any] =
    evolutionSteps
      .collect {
        case FieldAdded(name, default) => (name, default)
      }
      .toMap
  private val madeOptionalAt: Map[String, Byte] =
    evolutionSteps
      .zipWithIndex
      .collect {
        case (FieldMadeOptional(name), idx) => (name, idx)
      }
      .map { case (name, idx) => (name, idx.toByte) }
      .toMap
  private val removedFields: Set[String] =
    evolutionSteps
      .collect {
        case FieldRemoved(name) => name
      }.toSet

  private val constructorNameToId = constructorMap.zipWithIndex.toMap
  private val constructorIdToName = constructorMap.zipWithIndex.map { case (name, id) => (id, name) }.toMap

  def codec: BinaryCodec[T] =
    BinaryCodec.define[T] {
      value =>
        for {
          _ <- writeByte(version)
          primaryOutput <- getOutput
          chunkedOutput = createChunkedOutput(primaryOutput)
          genericValue = gen.to(value)
          state <- getSerializerState
          typeRegistry <- getOutputTypeRegistry
          initialState = ChunkedSerState(
            state,
            typeRegistry,
            lastIndexPerChunk = Map.empty,
            fieldIndices = Map.empty,
            constructorNameToId,
            constructorIdToName,
            typeDescription,
            readConstructorName = None,
            transientFields = Map.empty // TODO
          )
          finalState <- Ser.fromEither(
            serializer.value.serialize(taggedTransients.tag(genericValue))
              .provideService(chunkedOutput)
              .getState
              .map(_._1)
              .either
              .runResult(initialState))
          _ <- chunkedOutput.writeEvolutionHeader(finalState.fieldIndices)
          _ <- chunkedOutput.writeOrderedChunks()
          _ <- setSerializerState(finalState.serializerState)
        } yield ()
    } {
      for {
        storedVersion <- readByte()
        primaryInput <- getInput
        chunkedInput <- createChunkedInput(primaryInput, storedVersion)
        state <- getDeserializerState
        typeRegistry <- getInputTypeRegistry
        initialState = ChunkedSerState(
          state,
          typeRegistry,
          lastIndexPerChunk = Map.empty,
          fieldIndices = Map.empty,
          constructorNameToId,
          constructorIdToName,
          typeDescription = classTag.runtimeClass.getName,
          readConstructorName = None,
          transientFields)
        result <- Deser.fromEither(deserializer.value.deserialize()
          .provideService(chunkedInput)
          .getState
          .either
          .runResult(initialState))
        (finalState, hlist) = result
        _ <- setDeserializerState(finalState.serializerState)
      } yield gen.from(taggedTransients.untag(hlist))
    }

  private def createChunkedOutput(primaryOutput: BinaryOutput): ChunkedOutput =
    if (version == 0) {
      // Simple mode: we serialize directly to the main stream
      new ChunkedOutput {
        override def outputFor(version: Byte): BinaryOutput = primaryOutput

        override def writeEvolutionHeader(fieldIndices: Map[String, FieldPosition]): Ser[Unit] = finishSerializer()

        override def writeOrderedChunks(): Ser[Unit] = finishSerializer()
      }
    } else {
      new ChunkedOutput {
        private val streams: Array[ByteArrayOutputStream] = (0 to version).map(_ => new ByteArrayOutputStream()).toArray
        private val outputs: Array[JavaStreamBinaryOutput] = streams.map(new JavaStreamBinaryOutput(_))

        override def outputFor(version: Byte): BinaryOutput = outputs(version)

        override def writeEvolutionHeader(fieldIndices: Map[String, FieldPosition]): Ser[Unit] = {
          (0 to version).foldLeft(finishSerializer()) { case (s, v) =>
            val serializedEvolutionStep = evolutionSteps(v) match {
              case InitialVersion =>
                val size = {
                  streams(v).flush()
                  streams(v).size()
                }
                Right(SerializedEvolutionStep.FieldAddedToNewChunk(size))
              case FieldAdded(_, _) =>
                val size = {
                  streams(v).flush()
                  streams(v).size()
                }
                Right(SerializedEvolutionStep.FieldAddedToNewChunk(size))
              case FieldMadeOptional(name) =>
                fieldIndices.get(name) match {
                  case Some(fieldPosition) =>
                    Right(SerializedEvolutionStep.FieldMadeOptional(fieldPosition))
                  case None =>
                    if (removedFields.contains(name)) {
                      Right(SerializedEvolutionStep.FieldMadeOptional(FieldPosition.removed))
                    } else {
                      Left(UnknownFieldReferenceInEvolutionStep(name))
                    }
                }
              case FieldRemoved(name) =>
                Right(SerializedEvolutionStep.FieldRemoved(name))
              case _ =>
                Right(SerializedEvolutionStep.UnknownEvolutionStep)
            }
            serializedEvolutionStep match {
              case Left(failure) =>
                s *> failSerializerWith(failure)
              case Right(step) =>
                s *> write[SerializedEvolutionStep](step)
            }
          }
        }

        override def writeOrderedChunks(): Ser[Unit] = {
          streams.foldLeft(finishSerializer()) {
            case (m, stream) => m *> writeBytes({
              stream.flush()
              stream.toByteArray
            })
          }
        }
      }
    }

  private def createChunkedInput(primaryInput: BinaryInput, storedVer: Byte): Deser[ChunkedInput] =
    if (storedVer == 0) {
      // Simple mode: deserializing directly from the input stream
      finishDeserializerWith(
        new ChunkedInput {
          override val storedVersion: Byte = storedVer

          override val madeOptionalAt: Map[FieldPosition, Byte] = Map.empty

          override val removedFields: Set[String] = Set.empty

          override def inputFor(version: Byte): Either[DesertFailure, BinaryInput] =
            if (version == 0) Right(primaryInput) else Left(DeserializingNonExistingChunk(version))
        })
    } else {
      for {
        serializedEvolutionSteps <- (0 to storedVer).foldLeft(finishDeserializerWith(Vector.empty[SerializedEvolutionStep])) {
          case (m, _) => m.flatMap { vec => read[SerializedEvolutionStep]().map(vec :+ _)
          }
        }
        chunks <- serializedEvolutionSteps.foldLeft(finishDeserializerWith(Vector.empty[Array[Byte]])) {
          case (m, SerializedEvolutionStep.FieldAddedToNewChunk(size)) => m.flatMap { vec =>
            readBytes(size).map(vec :+ _)
          }
          case (m, _) => m.map { vec => vec :+ Array[Byte](0) }
        }
      } yield new ChunkedInput {
        private val streams = chunks.map(new ByteArrayInputStream(_)).toArray
        private val inputs = streams.map(new JavaStreamBinaryInput(_))

        override val storedVersion: Byte = storedVer

        override val madeOptionalAt: Map[FieldPosition, Byte] =
          serializedEvolutionSteps
            .zipWithIndex
            .collect { case (SerializedEvolutionStep.FieldMadeOptional(position), idx) => (position, idx.toByte) }
            .toMap

        override val removedFields: Set[String] =
          serializedEvolutionSteps
            .collect { case SerializedEvolutionStep.FieldRemoved(name) => name }
            .toSet

        override def inputFor(version: Byte): Either[DesertFailure, BinaryInput] =
          if (version < inputs.length) {
            Right(inputs(version))
          } else {
            Left(DeserializingNonExistingChunk(version))
          }
      }
    }
}

object AdtCodec {
  case class FieldPosition(chunk: Byte, position: Byte) {
    val toByte: Byte = if (chunk == 0) (-position).toByte else chunk
  }

  object FieldPosition {
    implicit val codec: BinaryCodec[FieldPosition] = BinaryCodec.from[FieldPosition](
      codecs.byteCodec.contramap(_.toByte),
      codecs.byteCodec.map { byte =>
        if (byte <= 0) FieldPosition(0, (-byte).toByte) else FieldPosition(byte, 0)
      }
    )

    val removed: FieldPosition = FieldPosition(128.toByte, 0)
  }

  sealed trait SerializedEvolutionStep

  object SerializedEvolutionStep {

    object Codes {
      val Unknown: Int = 0
      val FieldMadeOptionalCode: Int = -1
      val FieldRemovedCode: Int = -2
    }

    case class FieldAddedToNewChunk(size: Int) extends SerializedEvolutionStep

    case class FieldMadeOptional(position: FieldPosition) extends SerializedEvolutionStep

    case class FieldRemoved(fieldName: String) extends SerializedEvolutionStep

    case object UnknownEvolutionStep extends SerializedEvolutionStep

    implicit val codec: BinaryCodec[SerializedEvolutionStep] =
      BinaryCodec.define[SerializedEvolutionStep] {
        case FieldAddedToNewChunk(size) => writeVarInt(size, optimizeForPositive = false)
        case FieldMadeOptional(position) => writeVarInt(Codes.FieldMadeOptionalCode, optimizeForPositive = false) *> write(position)
        case FieldRemoved(fieldName) => writeVarInt(Codes.FieldRemovedCode, optimizeForPositive = false) *> write(DeduplicatedString(fieldName))
        case UnknownEvolutionStep => writeVarInt(Codes.Unknown, optimizeForPositive = false)
      } {
        for {
          code <- readVarInt(optimizeForPositive = false)
          result <- code match {
            case Codes.Unknown => finishDeserializerWith(UnknownEvolutionStep)
            case Codes.FieldMadeOptionalCode => read[FieldPosition]().map(FieldMadeOptional.apply)
            case Codes.FieldRemovedCode => read[DeduplicatedString]().map(_.string).map(FieldRemoved.apply)
            case size if size > 0 => finishDeserializerWith(FieldAddedToNewChunk(size))
            case _ => failDeserializerWith(UnknownSerializedEvolutionStep(code))
          }
        } yield result
      }
  }

  trait ChunkedOutput {
    def outputFor(version: Byte): BinaryOutput

    def writeEvolutionHeader(fieldIndices: Map[String, FieldPosition]): Ser[Unit]

    def writeOrderedChunks(): Ser[Unit]
  }

  case class ChunkedSerState(serializerState: SerializerState,
                             typeRegistry: TypeRegistry,
                             lastIndexPerChunk: Map[Byte, Byte],
                             fieldIndices: Map[String, FieldPosition],
                             constructorNameToId: Map[String, Int],
                             constructorIdToName: Map[Int, String],
                             typeDescription: String,
                             readConstructorName: Option[String],
                             transientFields: Map[Symbol, Any]
                            )

  type ChunkedSer[T] = ZPure[Nothing, ChunkedSerState, ChunkedSerState, ChunkedOutput, DesertFailure, T]

  object ChunkedSerOps {
    final def getChunkedOutput: ChunkedSer[ChunkedOutput] = ZPure.service[ChunkedSerState, ChunkedOutput]
    final def getChunkedState: ChunkedSer[ChunkedSerState] = ZPure.get
    final def setChunkedState(newState: ChunkedSerState): ChunkedSer[Unit] = ZPure.set(newState)

    final def fromEither[T](value: Either[DesertFailure, T]): ChunkedSer[T] = ZPure.succeed(value).absolve
    final def pure[T](value: T): ChunkedSer[T] = fromEither(Right[DesertFailure, T](value))
    final def unit: ChunkedSer[Unit] = pure(())
    final def failWith[T](failure: DesertFailure): ChunkedSer[T] = fromEither(Left(failure))

    final def fromSer[T](value: Ser[T], output: BinaryOutput): ChunkedSer[T] =
      for {
        chunkedState <- getChunkedState
        runResult <- fromEither(value
          .provideService(SerializationEnv(output, chunkedState.typeRegistry))
          .getState
          .either
          .runResult(chunkedState.serializerState))
        (resultState, result) = runResult
        _ <- setChunkedState(chunkedState.copy(serializerState = resultState))
      } yield result

    final def recordFieldIndex(fieldName: String, chunk: Byte): ChunkedSer[Unit] = {
      ZPure.modify { state =>
        state.lastIndexPerChunk.get(chunk) match {
          case Some(lastIndex) =>
            val newIndex: Byte = (lastIndex + 1).toByte
            ((), state.copy(
              lastIndexPerChunk = state.lastIndexPerChunk.updated(chunk, newIndex),
              fieldIndices = state.fieldIndices + (fieldName -> FieldPosition(chunk, newIndex))
            ))
          case None =>
            ((), state.copy(
              lastIndexPerChunk = state.lastIndexPerChunk + (chunk -> 0),
              fieldIndices = state.fieldIndices + (fieldName -> FieldPosition(chunk, 0))
            ))
        }
      }
    }

    def getConstructorId(typeName: String): ChunkedSer[Int] =
      for {
        state <- getChunkedState
        result <- state.constructorNameToId.get(typeName) match {
          case Some(id) => pure[Int](id)
          case None => fromEither(Left(InvalidConstructorName(typeName, state.typeDescription)))
        }
      } yield result
  }

  trait ChunkedBinarySerializer[T] {
    def serialize(value: T): ChunkedSer[Unit]
  }

  trait ChunkedInput {
    val storedVersion: Byte
    val madeOptionalAt: Map[FieldPosition, Byte]
    val removedFields: Set[String]

    def inputFor(version: Byte): Either[DesertFailure, BinaryInput]
  }

  type ChunkedDeser[T] = ZPure[Nothing, ChunkedSerState, ChunkedSerState, ChunkedInput, DesertFailure, T]

  object ChunkedDeserOps {
    final def getChunkedInput: ChunkedDeser[ChunkedInput] = ZPure.service
    final def getChunkedState: ChunkedDeser[ChunkedSerState] = ZPure.get
    final def setChunkedState(newState: ChunkedSerState): ChunkedDeser[Unit] = ZPure.set(newState)

    final def fromEither[T](value: Either[DesertFailure, T]): ChunkedDeser[T] = ZPure.succeed(value).absolve
    final def pure[T](value: T): ChunkedDeser[T] = fromEither(Right[DesertFailure, T](value))
    final def failWith[T](failure: DesertFailure): ChunkedDeser[T] = fromEither(Left(failure))

    final def fromDeser[T](value: Deser[T], input: BinaryInput): ChunkedDeser[T] =
      for {
        chunkedState <- getChunkedState
        runResult <- fromEither(value
          .provideService(DeserializationEnv(input, chunkedState.typeRegistry))
          .getState
          .either
          .runResult(chunkedState.serializerState))
        (resultState, result) = runResult
        _ <- setChunkedState(chunkedState.copy(serializerState = resultState))
      } yield result

    final def recordFieldIndex(fieldName: String, chunk: Byte): ChunkedDeser[FieldPosition] =
      for {
        state <- getChunkedState
        (newState, position) = state.lastIndexPerChunk.get(chunk) match {
          case Some(lastIndex) =>
            val newIndex: Byte = (lastIndex + 1).toByte
            (state.copy(
              lastIndexPerChunk = state.lastIndexPerChunk.updated(chunk, newIndex),
              fieldIndices = state.fieldIndices + (fieldName -> FieldPosition(chunk, newIndex))
            ), FieldPosition(chunk, newIndex))
          case None =>
            (state.copy(
              lastIndexPerChunk = state.lastIndexPerChunk + (chunk -> 0),
              fieldIndices = state.fieldIndices + (fieldName -> FieldPosition(chunk, 0))
            ), FieldPosition(chunk, 0))
        }
        _ <- setChunkedState(newState)
      } yield position

    final def getConstructorName(id: Int): ChunkedDeser[String] =
      for {
        state <- getChunkedState
        result <- state.constructorIdToName.get(id) match {
          case Some(name) => pure[String](name)
          case None => fromEither(Left(InvalidConstructorId(id, state.typeDescription)))
        }
      } yield result

    final def readOrGetConstructorName(input: BinaryInput): ChunkedDeser[String] =
      for {
        state <- getChunkedState
        constructorName <- state.readConstructorName match {
          case Some(value) => pure(value)
          case None =>
            for {
              constructorId <- ChunkedDeserOps.fromDeser(readVarInt(optimizeForPositive = true), input)
              constructorName <- ChunkedDeserOps.getConstructorName(constructorId)
              _ <- setChunkedState(state.copy(readConstructorName = Some(constructorName)))
            } yield constructorName
        }
      } yield constructorName
  }

  trait ChunkedBinaryDeserializer[T] {
    def deserialize(): ChunkedDeser[T]
  }
}