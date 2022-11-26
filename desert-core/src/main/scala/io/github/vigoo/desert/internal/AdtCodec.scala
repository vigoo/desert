package io.github.vigoo.desert.internal

import io.github.vigoo.desert._
import io.github.vigoo.desert.Evolution._
import io.github.vigoo.desert.custom._
import zio.prelude.fx.ZPure

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import scala.annotation.nowarn

class AdtCodec[T, BuilderState](
    evolutionSteps: Vector[Evolution],
    typeName: String,
    constructors: Vector[String],
    transientFields: Map[Symbol, Any],
    getSerializationCommands: T => List[AdtCodec.SerializationCommand],
    deserializationCommands: List[AdtCodec.DeserializationCommand[BuilderState]],
    initialBuilderState: BuilderState,
    materialize: BuilderState => Either[DesertFailure, T]
) extends BinaryCodec[T] {
  import AdtCodec._

  private val version: Byte                       = (evolutionSteps.size - 1).toByte
  private val fieldGenerations: Map[String, Byte] =
    evolutionSteps.zipWithIndex
      .collect { case (FieldAdded(name, _), idx) =>
        (name, idx)
      }
      .map { case (name, idx) => (name, idx.toByte) }
      .toMap
  private val fieldDefaults: Map[String, Any]     =
    evolutionSteps.collect { case FieldAdded(name, default) =>
      (name, default)
    }.toMap
  private val madeOptionalAt: Map[String, Byte]   =
    evolutionSteps.zipWithIndex
      .collect { case (FieldMadeOptional(name), idx) =>
        (name, idx)
      }
      .map { case (name, idx) => (name, idx.toByte) }
      .toMap
  private val removedFields: Set[String]          =
    evolutionSteps.collect { case FieldRemoved(name) =>
      name
    }.toSet

  private val constructorNameToId = constructors.zipWithIndex.toMap
  private val constructorIdToName = constructors.zipWithIndex.map { case (name, id) => (id, name) }.toMap

  override def serialize(value: T): Ser[Unit] =
    for {
      _             <- writeByte(version)
      primaryOutput <- getOutput
      chunkedOutput  = createChunkedOutput(primaryOutput)
      state         <- getSerializerState
      typeRegistry  <- getOutputTypeRegistry
      initialState   = ChunkedSerState(
                         state,
                         typeRegistry,
                         lastIndexPerChunk = Map.empty,
                         fieldIndices = Map.empty,
                         constructorNameToId,
                         constructorIdToName,
                         typeDescription = typeName,
                         readConstructorName = None,
                         transientFields
                       )
      commands       = getSerializationCommands(value)
      finalState    <- Ser.fromEither(
                         ChunkedSerOps
                           .foreach(commands)(serialize)
                           .provideService(chunkedOutput)
                           .getState
                           .map(_._1)
                           .either
                           .runResult(initialState)
                       )
      _             <- chunkedOutput.writeEvolutionHeader(finalState.fieldIndices)
      _             <- chunkedOutput.writeOrderedChunks()
      _             <- setSerializerState(finalState.serializerState)
    } yield ()

  override def deserialize(): Deser[T] =
    for {
      storedVersion                  <- readByte()
      primaryInput                   <- getInput
      chunkedInput                   <- createChunkedInput(primaryInput, storedVersion)
      state                          <- getDeserializerState
      typeRegistry                   <- getInputTypeRegistry
      initialState                    = ChunkedSerState(
                                          state,
                                          typeRegistry,
                                          lastIndexPerChunk = Map.empty,
                                          fieldIndices = Map.empty,
                                          constructorNameToId,
                                          constructorIdToName,
                                          typeDescription = typeName,
                                          readConstructorName = None,
                                          transientFields
                                        )
      result                         <- Deser.fromEither(
                                          ChunkedDeserOps
                                            .foldLeft(deserializationCommands)(initialBuilderState)(deserialize)
                                            .provideService(chunkedInput)
                                            .getState
                                            .either
                                            .runResult(initialState)
                                        )
      (finalState, finalBuilderState) = result
      _                              <- setDeserializerState(finalState.serializerState)
      materializedResult             <- Deser.fromEither(materialize(finalBuilderState))
    } yield materializedResult

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
        private val streams: Array[ByteArrayOutputStream]  = (0 to version).map(_ => new ByteArrayOutputStream()).toArray
        private val outputs: Array[JavaStreamBinaryOutput] = streams.map(new JavaStreamBinaryOutput(_))

        override def outputFor(version: Byte): BinaryOutput = outputs(version)

        override def writeEvolutionHeader(fieldIndices: Map[String, FieldPosition]): Ser[Unit] =
          (0 to version).foldLeft(finishSerializer()) { case (s, v) =>
            val serializedEvolutionStep = evolutionSteps(v) match {
              case InitialVersion          =>
                val size = {
                  streams(v).flush()
                  streams(v).size()
                }
                Right(SerializedEvolutionStep.FieldAddedToNewChunk(size))
              case FieldAdded(_, _)        =>
                val size = {
                  streams(v).flush()
                  streams(v).size()
                }
                Right(SerializedEvolutionStep.FieldAddedToNewChunk(size))
              case FieldMadeOptional(name) =>
                fieldIndices.get(name) match {
                  case Some(fieldPosition) =>
                    Right(SerializedEvolutionStep.FieldMadeOptional(fieldPosition))
                  case None                =>
                    if (removedFields.contains(name)) {
                      Right(SerializedEvolutionStep.FieldMadeOptional(FieldPosition.removed))
                    } else {
                      Left(DesertFailure.UnknownFieldReferenceInEvolutionStep(name))
                    }
                }
              case FieldRemoved(name)      =>
                Right(SerializedEvolutionStep.FieldRemoved(name))
              case _                       =>
                Right(SerializedEvolutionStep.UnknownEvolutionStep)
            }
            serializedEvolutionStep match {
              case Left(failure) =>
                s *> failSerializerWith(failure)
              case Right(step)   =>
                s *> write[SerializedEvolutionStep](step)
            }
          }

        override def writeOrderedChunks(): Ser[Unit] =
          streams.foldLeft(finishSerializer()) { case (m, stream) =>
            m *> writeBytes({
              stream.flush()
              stream.toByteArray
            })
          }
      }
    }

  private def createChunkedInput(primaryInput: BinaryInput, storedVer: Byte): Deser[ChunkedInput] =
    if (storedVer == 0) {
      // Simple mode: deserializing directly from the input stream
      finishDeserializerWith(new ChunkedInput {
        override val storedVersion: Byte = storedVer

        override val madeOptionalAt: Map[FieldPosition, Byte] = Map.empty

        override val removedFields: Set[String] = Set.empty

        override def inputFor(version: Byte): Either[DesertFailure, BinaryInput] =
          if (version == 0) Right(primaryInput) else Left(DesertFailure.DeserializingNonExistingChunk(version))
      })
    } else {
      for {
        serializedEvolutionSteps <-
          (0 to storedVer).foldLeft(finishDeserializerWith(Vector.empty[SerializedEvolutionStep])) { case (m, _) =>
            m.flatMap(vec => read[SerializedEvolutionStep]().map(vec :+ _))
          }
        chunks                   <- serializedEvolutionSteps.foldLeft(finishDeserializerWith(Vector.empty[Array[Byte]])) {
                                      case (m, SerializedEvolutionStep.FieldAddedToNewChunk(size)) =>
                                        m.flatMap { vec =>
                                          readBytes(size).map(vec :+ _)
                                        }
                                      case (m, _)                                                  => m.map(vec => vec :+ Array[Byte](0))
                                    }
      } yield new ChunkedInput {
        private val streams = chunks.map(new ByteArrayInputStream(_)).toArray
        private val inputs  = streams.map(new JavaStreamBinaryInput(_))

        override val storedVersion: Byte = storedVer

        override val madeOptionalAt: Map[FieldPosition, Byte] =
          serializedEvolutionSteps.zipWithIndex.collect {
            case (SerializedEvolutionStep.FieldMadeOptional(position), idx) => (position, idx.toByte)
          }.toMap

        override val removedFields: Set[String] =
          serializedEvolutionSteps.collect { case SerializedEvolutionStep.FieldRemoved(name) => name }.toSet

        override def inputFor(version: Byte): Either[DesertFailure, BinaryInput] =
          if (version < inputs.length) {
            Right(inputs(version))
          } else {
            Left(DesertFailure.DeserializingNonExistingChunk(version))
          }
      }
    }

  private def readOptionalFieldIfExists[H](
      fieldName: String,
      headCodec: BinaryCodec[H],
      optHeadCodec: BinaryCodec[Option[H]]
  ): ChunkedDeser[Option[H]] =
    ChunkedDeserOps.getChunkedInput.flatMap { chunkedInput =>
      if (chunkedInput.removedFields.contains(fieldName)) {
        ChunkedDeserOps.pure(None)
      } else {
        val chunk    = fieldGenerations.getOrElse(fieldName, 0: Byte)
        val optSince = madeOptionalAt.getOrElse(fieldName, 0: Byte)

        ChunkedDeserOps.recordFieldIndex(fieldName, chunk).flatMap { _ =>
          if (chunkedInput.storedVersion < chunk) {
            // This field was not serialized
            fieldDefaults.get(fieldName) match {
              case Some(value) =>
                if (optSince <= chunk) {
                  // It was originally Option[H]
                  ChunkedDeserOps.pure(value.asInstanceOf[Option[H]])
                } else {
                  // It was made optional after it was added
                  ChunkedDeserOps.pure(Some(value.asInstanceOf[H]))
                }
              case None        =>
                ChunkedDeserOps.failWith(
                  DesertFailure.DeserializationFailure(
                    s"Field $fieldName is not in the stream and does not have default value",
                    None
                  )
                )

            }
          } else {
            // This field was serialized
            if (chunkedInput.storedVersion < optSince) {
              // Expect H in the input stream and wrap with Some()
              for {
                input     <- ChunkedDeserOps.fromEither(chunkedInput.inputFor(chunk))
                headValue <- ChunkedDeserOps.fromDeser(headCodec.deserialize(), input)
              } yield Some(headValue)
            } else {
              // Expect Option[H] in the input stream
              for {
                input     <- ChunkedDeserOps.fromEither(chunkedInput.inputFor(chunk))
                headValue <- ChunkedDeserOps.fromDeser(optHeadCodec.deserialize(), input)
              } yield headValue
            }
          }
        }
      }
    }

  private def readFieldIfExists[H](fieldName: String, headCodec: BinaryCodec[H]): ChunkedDeser[H] =
    ChunkedDeserOps.getChunkedInput.flatMap { chunkedInput =>
      // Check if field was removed
      if (chunkedInput.removedFields.contains(fieldName)) {
        ChunkedDeserOps.failWith(DesertFailure.FieldRemovedInSerializedVersion(fieldName))
      } else {
        val chunk = fieldGenerations.getOrElse(fieldName, 0: Byte)
        ChunkedDeserOps.recordFieldIndex(fieldName, chunk).flatMap { fieldPosition =>
          if (chunkedInput.storedVersion < chunk) {
            // Field was not serialized
            fieldDefaults.get(fieldName) match {
              case Some(value) =>
                ChunkedDeserOps.pure(value.asInstanceOf[H])
              case None        =>
                ChunkedDeserOps.failWith(DesertFailure.FieldWithoutDefaultValueIsMissing(fieldName))
            }
          } else {
            // Field was serialized

            if (chunkedInput.madeOptionalAt.contains(fieldPosition)) {
              // The field was made optional in by a newer version, reading as Option[H]
              for {
                input     <- ChunkedDeserOps.fromEither(chunkedInput.inputFor(chunk))
                isDefined <- ChunkedDeserOps.fromDeser(booleanCodec.deserialize(), input)
                headValue <- if (isDefined) {
                               ChunkedDeserOps.fromDeser(headCodec.deserialize(), input)
                             } else {
                               ChunkedDeserOps.failWith(DesertFailure.NonOptionalFieldSerializedAsNone(fieldName))
                             }
              } yield headValue
            } else {
              // Default case, reading the field from the given chunk
              for {
                input     <- ChunkedDeserOps.fromEither(chunkedInput.inputFor(chunk))
                headValue <- ChunkedDeserOps.fromDeser(headCodec.deserialize(), input)
              } yield headValue
            }
          }
        }
      }
    }

  private def serialize(command: SerializationCommand): ChunkedSer[Unit] =
    command match {
      case SerializationCommand.WriteField(fieldName, value, codec)             =>
        for {
          chunkedOutput <- ChunkedSerOps.getChunkedOutput
          chunk          = fieldGenerations.getOrElse(fieldName, 0: Byte)
          output         = chunkedOutput.outputFor(chunk)
          _             <- ChunkedSerOps.fromSer(
                             codec().serialize(value),
                             output
                           )
          _             <- ChunkedSerOps.recordFieldIndex(fieldName, chunk)
        } yield ()
      case SerializationCommand.WriteConstructor(constructorName, value, codec) =>
        for {
          chunkedOutput <- ChunkedSerOps.getChunkedOutput
          output         = chunkedOutput.outputFor(0)
          constructorId <- ChunkedSerOps.getConstructorId(constructorName)
          _             <- ChunkedSerOps.fromSer(
                             writeVarInt(constructorId, optimizeForPositive = true) *>
                               codec().serialize(value),
                             output
                           )
        } yield ()
      case SerializationCommand.Fail(failure)                                   =>
        ChunkedSerOps.failWith(failure)
    }

  @nowarn("msg=.*eliminated by erasure") private def deserialize(
      builderState: BuilderState,
      command: DeserializationCommand[BuilderState]
  ): ChunkedDeser[BuilderState] =
    command match {
      case DeserializationCommand.Read(fieldName, codec, store) =>
        readFieldIfExists(fieldName, codec()).map(value => store(value, builderState))

      case DeserializationCommand.ReadOptional(fieldName, codec, optCodec, store) =>
        readOptionalFieldIfExists(fieldName, codec(), optCodec()).map(value => store(value, builderState))

      case readTransient: DeserializationCommand.ReadTransient[Any, BuilderState] =>
        for {
          chunkedState <- ChunkedDeserOps.getChunkedState
          headValue    <- chunkedState.transientFields.get(Symbol(readTransient.fieldName)) match {
                            case Some(value) =>
                              ChunkedDeserOps.pure(value)
                            case None        =>
                              ChunkedDeserOps.failWith(
                                DesertFailure.DeserializationFailure(
                                  s"Illegal state while processing transient field ${readTransient.fieldName}",
                                  None
                                )
                              )
                          }
        } yield readTransient.store(headValue, builderState)

      case DeserializationCommand.ReadConstructor(expectedConstructorName, codec, store) =>
        for {
          chunkedInput    <- ChunkedDeserOps.getChunkedInput
          input           <- ChunkedDeserOps.fromEither(chunkedInput.inputFor(0))
          constructorName <- ChunkedDeserOps.readOrGetConstructorName(input)
          result          <- if (expectedConstructorName == constructorName) {
                               ChunkedDeserOps
                                 .fromDeser(codec().deserialize(), input)
                                 .map(headValue => store(headValue, builderState))
                             } else {
                               ChunkedDeserOps.pure(builderState)
                             }
        } yield result
    }
}

object AdtCodec {

  final case class FieldPosition(chunk: Byte, position: Byte) {
    val toByte: Byte = if (chunk == 0) (-position).toByte else chunk
  }

  object FieldPosition {
    implicit val codec: BinaryCodec[FieldPosition] = BinaryCodec.from[FieldPosition](
      byteCodec.contramap(_.toByte),
      byteCodec.map { byte =>
        if (byte <= 0) FieldPosition(0, (-byte).toByte) else FieldPosition(byte, 0)
      }
    )

    val removed: FieldPosition = FieldPosition(128.toByte, 0)
  }

  sealed trait SerializedEvolutionStep

  object SerializedEvolutionStep {

    object Codes {
      val Unknown: Int               = 0
      val FieldMadeOptionalCode: Int = -1
      val FieldRemovedCode: Int      = -2
    }

    final case class FieldAddedToNewChunk(size: Int) extends SerializedEvolutionStep

    final case class FieldMadeOptional(position: FieldPosition) extends SerializedEvolutionStep

    final case class FieldRemoved(fieldName: String) extends SerializedEvolutionStep

    case object UnknownEvolutionStep extends SerializedEvolutionStep

    implicit val codec: BinaryCodec[SerializedEvolutionStep] =
      BinaryCodec.define[SerializedEvolutionStep] {
        case FieldAddedToNewChunk(size)  => writeVarInt(size, optimizeForPositive = false)
        case FieldMadeOptional(position) =>
          writeVarInt(Codes.FieldMadeOptionalCode, optimizeForPositive = false) *> write(position)
        case FieldRemoved(fieldName)     =>
          writeVarInt(Codes.FieldRemovedCode, optimizeForPositive = false) *> write(DeduplicatedString(fieldName))
        case UnknownEvolutionStep        => writeVarInt(Codes.Unknown, optimizeForPositive = false)
      } {
        for {
          code   <- readVarInt(optimizeForPositive = false)
          result <- code match {
                      case Codes.Unknown               => finishDeserializerWith(UnknownEvolutionStep)
                      case Codes.FieldMadeOptionalCode => read[FieldPosition]().map(FieldMadeOptional.apply)
                      case Codes.FieldRemovedCode      => read[DeduplicatedString]().map(_.string).map(FieldRemoved.apply)
                      case size if size > 0            => finishDeserializerWith(FieldAddedToNewChunk(size))
                      case _                           => failDeserializerWith(DesertFailure.UnknownSerializedEvolutionStep(code))
                    }
        } yield result
      }
  }

  trait ChunkedOutput {
    def outputFor(version: Byte): BinaryOutput

    def writeEvolutionHeader(fieldIndices: Map[String, FieldPosition]): Ser[Unit]

    def writeOrderedChunks(): Ser[Unit]
  }

  final case class ChunkedSerState(
      serializerState: SerializerState,
      typeRegistry: TypeRegistry,
      lastIndexPerChunk: Map[Byte, Byte],
      fieldIndices: Map[String, FieldPosition],
      constructorNameToId: Map[String, Int],
      constructorIdToName: Map[Int, String],
      typeDescription: String,
      readConstructorName: Option[String],
      transientFields: Map[Symbol, Any]
  )

  type ChunkedSer[+T] = ZPure[Nothing, ChunkedSerState, ChunkedSerState, ChunkedOutput, DesertFailure, T]

  object ChunkedSerOps {
    final def getChunkedOutput: ChunkedSer[ChunkedOutput] = ZPure.service[ChunkedSerState, ChunkedOutput]

    final def getChunkedState: ChunkedSer[ChunkedSerState] = ZPure.get

    final def setChunkedState(newState: ChunkedSerState): ChunkedSer[Unit] = ZPure.set(newState)

    final def fromEither[T](value: Either[DesertFailure, T]): ChunkedSer[T] = ZPure.succeed(value).absolve

    final def pure[T](value: T): ChunkedSer[T] = fromEither(Right[DesertFailure, T](value))

    final def unit: ChunkedSer[Unit] = pure(())

    final def failWith[T](failure: DesertFailure): ChunkedSer[T] = fromEither(Left(failure))

    final def foreach[T](values: List[T])(f: T => ChunkedSer[Unit]): ChunkedSer[Unit] =
      ZPure.forEach(values)(f).unit

    final def fromSer[T](value: Ser[T], output: BinaryOutput): ChunkedSer[T] =
      for {
        chunkedState         <- getChunkedState
        runResult            <- fromEither(
                                  value
                                    .provideService(SerializationEnv(output, chunkedState.typeRegistry))
                                    .getState
                                    .either
                                    .runResult(chunkedState.serializerState)
                                )
        (resultState, result) = runResult
        _                    <- setChunkedState(chunkedState.copy(serializerState = resultState))
      } yield result

    final def recordFieldIndex(fieldName: String, chunk: Byte): ChunkedSer[Unit] =
      ZPure.modify { state =>
        state.lastIndexPerChunk.get(chunk) match {
          case Some(lastIndex) =>
            val newIndex: Byte = (lastIndex + 1).toByte
            (
              (),
              state.copy(
                lastIndexPerChunk = state.lastIndexPerChunk.updated(chunk, newIndex),
                fieldIndices = state.fieldIndices + (fieldName -> FieldPosition(chunk, newIndex))
              )
            )
          case None            =>
            (
              (),
              state.copy(
                lastIndexPerChunk = state.lastIndexPerChunk + (chunk -> 0),
                fieldIndices = state.fieldIndices + (fieldName       -> FieldPosition(chunk, 0))
              )
            )
        }
      }

    final def getConstructorId(typeName: String): ChunkedSer[Int] =
      for {
        state  <- getChunkedState
        result <- state.constructorNameToId.get(typeName) match {
                    case Some(id) => pure[Int](id)
                    case None     => fromEither(Left(DesertFailure.InvalidConstructorName(typeName, state.typeDescription)))
                  }
      } yield result
  }

  trait ChunkedInput {
    val storedVersion: Byte
    val madeOptionalAt: Map[FieldPosition, Byte]
    val removedFields: Set[String]

    def inputFor(version: Byte): Either[DesertFailure, BinaryInput]
  }

  type ChunkedDeser[+T] = ZPure[Nothing, ChunkedSerState, ChunkedSerState, ChunkedInput, DesertFailure, T]

  object ChunkedDeserOps {
    final def getChunkedInput: ChunkedDeser[ChunkedInput] = ZPure.service

    final def getChunkedState: ChunkedDeser[ChunkedSerState] = ZPure.get

    final def setChunkedState(newState: ChunkedSerState): ChunkedDeser[Unit] = ZPure.set(newState)

    final def fromEither[T](value: Either[DesertFailure, T]): ChunkedDeser[T] = ZPure.succeed(value).absolve

    final def pure[T](value: T): ChunkedDeser[T] = fromEither(Right[DesertFailure, T](value))

    final def failWith[T](failure: DesertFailure): ChunkedDeser[T] = fromEither(Left(failure))

    final def foldLeft[S, A](values: List[A])(init: S)(f: (S, A) => ChunkedDeser[S]): ChunkedDeser[S] =
      values.foldLeft(pure(init)) { case (getState, value) =>
        getState.flatMap(f(_, value))
      }

    final def fromDeser[T](value: Deser[T], input: BinaryInput): ChunkedDeser[T] =
      for {
        chunkedState         <- getChunkedState
        runResult            <- fromEither(
                                  value
                                    .provideService(DeserializationEnv(input, chunkedState.typeRegistry))
                                    .getState
                                    .either
                                    .runResult(chunkedState.serializerState)
                                )
        (resultState, result) = runResult
        _                    <- setChunkedState(chunkedState.copy(serializerState = resultState))
      } yield result

    final def recordFieldIndex(fieldName: String, chunk: Byte): ChunkedDeser[FieldPosition] =
      for {
        state               <- getChunkedState
        (newState, position) = state.lastIndexPerChunk.get(chunk) match {
                                 case Some(lastIndex) =>
                                   val newIndex: Byte = (lastIndex + 1).toByte
                                   (
                                     state.copy(
                                       lastIndexPerChunk = state.lastIndexPerChunk.updated(chunk, newIndex),
                                       fieldIndices = state.fieldIndices + (fieldName -> FieldPosition(chunk, newIndex))
                                     ),
                                     FieldPosition(chunk, newIndex)
                                   )
                                 case None            =>
                                   (
                                     state.copy(
                                       lastIndexPerChunk = state.lastIndexPerChunk + (chunk -> 0),
                                       fieldIndices = state.fieldIndices + (fieldName       -> FieldPosition(chunk, 0))
                                     ),
                                     FieldPosition(chunk, 0)
                                   )
                               }
        _                   <- setChunkedState(newState)
      } yield position

    final def getConstructorName(id: Int): ChunkedDeser[String] =
      for {
        state  <- getChunkedState
        result <- state.constructorIdToName.get(id) match {
                    case Some(name) => pure[String](name)
                    case None       => fromEither(Left(DesertFailure.InvalidConstructorId(id, state.typeDescription)))
                  }
      } yield result

    final def readOrGetConstructorName(input: BinaryInput): ChunkedDeser[String] =
      for {
        state           <- getChunkedState
        constructorName <- state.readConstructorName match {
                             case Some(value) => pure(value)
                             case None        =>
                               for {
                                 constructorId   <-
                                   ChunkedDeserOps.fromDeser(readVarInt(optimizeForPositive = true), input)
                                 constructorName <- ChunkedDeserOps.getConstructorName(constructorId)
                                 _               <- setChunkedState(state.copy(readConstructorName = Some(constructorName)))
                               } yield constructorName
                           }
      } yield constructorName
  }

  sealed trait SerializationCommand
  object SerializationCommand {
    final case class WriteField[T](fieldName: String, value: T, codec: () => BinaryCodec[T])
        extends SerializationCommand
    final case class WriteConstructor[T](constructorName: String, value: T, codec: () => BinaryCodec[T])
        extends SerializationCommand
    final case class Fail(failure: DesertFailure) extends SerializationCommand
  }

  sealed trait DeserializationCommand[BuilderState]
  object DeserializationCommand {
    final case class Read[T, BuilderState](
        fieldName: String,
        codec: () => BinaryCodec[T],
        store: (T, BuilderState) => BuilderState
    ) extends DeserializationCommand[BuilderState]

    final case class ReadOptional[T, BuilderState](
        fieldName: String,
        codec: () => BinaryCodec[T],
        optCodec: () => BinaryCodec[Option[T]],
        store: (Option[T], BuilderState) => BuilderState
    ) extends DeserializationCommand[BuilderState]

    final case class ReadTransient[T, BuilderState](fieldName: String, store: (T, BuilderState) => BuilderState)
        extends DeserializationCommand[BuilderState]

    final case class ReadConstructor[T, BuilderState](
        expectedConstructorName: String,
        codec: () => BinaryCodec[T],
        store: (T, BuilderState) => BuilderState
    ) extends DeserializationCommand[BuilderState]
  }
}
