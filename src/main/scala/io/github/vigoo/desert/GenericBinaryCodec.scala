package io.github.vigoo.desert

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

import cats.data.{ReaderT, StateT}
import cats.instances.either._
import cats.syntax.flatMap._
import io.github.vigoo.desert.BinaryDeserializer.Deser
import io.github.vigoo.desert.BinaryDeserializerOps._
import io.github.vigoo.desert.BinarySerializer.Ser
import io.github.vigoo.desert.BinarySerializerOps._
import io.github.vigoo.desert.codecs._
import io.github.vigoo.desert.GenericBinaryCodec._
import shapeless._
import shapeless.labelled._

trait LowerPriorityGenericDerivationApi {
  implicit def hlistDeserializer[K <: Symbol, H, T <: HList](implicit witness: Witness.Aux[K],
                                                             headCodec: Lazy[BinaryCodec[H]],
                                                             tailCodec: ChunkedBinaryDeserializer[T]): ChunkedBinaryDeserializer[FieldType[K, H] :: T]
}

trait GenericDerivationApi extends LowerPriorityGenericDerivationApi {
  implicit val hnilSerializer: ChunkedBinarySerializer[HNil]
  implicit val hnilDeserializer: ChunkedBinaryDeserializer[HNil]
  implicit val cnilCodec: BinaryCodec[CNil]

  implicit def hlistSerializer[K <: Symbol, H, T <: HList](implicit witness: Witness.Aux[K],
                                                           headCodec: Lazy[BinaryCodec[H]],
                                                           tailCodec: ChunkedBinarySerializer[T]): ChunkedBinarySerializer[FieldType[K, H] :: T]


  implicit def hlistOptionalDeserializer[K <: Symbol, H, T <: HList](implicit witness: Witness.Aux[K],
                                                                     headCodec: Lazy[BinaryCodec[H]],
                                                                     optHeadCodec: Lazy[BinaryCodec[Option[H]]],
                                                                     tailCodec: ChunkedBinaryDeserializer[T]): ChunkedBinaryDeserializer[FieldType[K, Option[H]] :: T]

  implicit def clistCodec[K <: Symbol, H, T <: Coproduct](implicit witness: Witness.Aux[K],
                                                          headCodec: Lazy[BinaryCodec[H]],
                                                          tailCodec: BinaryCodec[T]): BinaryCodec[FieldType[K, H] :+: T]

  def derive[T, H](implicit gen: LabelledGeneric.Aux[T, H],
                   hlistSerializer: Lazy[ChunkedBinarySerializer[H]],
                   hlistDeserializer: Lazy[ChunkedBinaryDeserializer[H]]): BinaryCodec[T]
}

class GenericBinaryCodec(evolutionSteps: Vector[Evolution]) extends GenericDerivationApi {
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

//  println("GenericBinaryCodec initialized")
//  println(s"version: $version")
//  println(s"fieldGenerations: $fieldGenerations")
//  println(s"fieldDefaults: $fieldDefaults")
//  println(s"madeOptionalAt: $madeOptionalAt")

  implicit val hnilSerializer: ChunkedBinarySerializer[HNil] =
    (_: HNil) => ChunkedSerOps.unit

  implicit val hnilDeserializer: ChunkedBinaryDeserializer[HNil] =
    () => ChunkedDeserOps.pure(HNil)

  implicit val cnilCodec: BinaryCodec[CNil] =
    new BinaryCodec[CNil] {
      override def deserialize(): Deser[CNil] = ???

      override def serialize(value: CNil): Ser[Unit] = ???
    }

  implicit def hlistSerializer[K <: Symbol, H, T <: HList](implicit witness: Witness.Aux[K],
                                                           headCodec: Lazy[BinaryCodec[H]],
                                                           tailCodec: ChunkedBinarySerializer[T]): ChunkedBinarySerializer[FieldType[K, H] :: T] = {
    case headValue :: tailValues =>
      for {
        chunkedOutput <- ChunkedSerOps.getChunkedOutput
        fieldName = witness.value.name
        chunk = fieldGenerations.getOrElse(fieldName, 0: Byte)
        output = chunkedOutput.outputFor(chunk)
        _ <- ChunkedSerOps.fromSer(
          headCodec.value.serialize(headValue),
          output
        )
        _ <- ChunkedSerOps.recordFieldIndex(fieldName, chunk)
        _ <- tailCodec.serialize(tailValues)
      } yield ()
  }

  private def readOptionalFieldIfExists[H](fieldName: String)
                                          (implicit headCodec: Lazy[BinaryCodec[H]],
                                           optHeadCodec: Lazy[BinaryCodec[Option[H]]]): ChunkedDeser[Option[H]] = {
    ChunkedDeserOps.getChunkedInput.flatMap { chunkedInput =>
      val chunk = fieldGenerations.getOrElse(fieldName, 0: Byte)
      val optSince = madeOptionalAt.getOrElse(fieldName, 0: Byte)

//      println(s"Reading optional field $fieldName from chunk $chunk")

      ChunkedDeserOps.recordFieldIndex(fieldName, chunk).flatMap { fieldPosition =>
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
            case None =>
              ChunkedDeserOps.failWith(DeserializationFailure(s"Field $fieldName is not in the stream and does not have default value", None))

          }
        } else {
          // This field was serialized
          if (chunkedInput.storedVersion < optSince) {
            // Expect H in the input stream and wrap with Some()
            for {
              input <- ChunkedDeserOps.fromEither(chunkedInput.inputFor(chunk))
              headValue <- ChunkedDeserOps.fromDeser(headCodec.value.deserialize(), input)
            } yield Some(headValue)
          } else {
            // Expect Option[H] in the input stream
            for {
              input <- ChunkedDeserOps.fromEither(chunkedInput.inputFor(chunk))
              headValue <- ChunkedDeserOps.fromDeser(optHeadCodec.value.deserialize(), input)
            } yield headValue
          }
        }
      }
    }
  }

  private def readFieldIfExists[H](fieldName: String)
                                  (implicit headCodec: Lazy[BinaryCodec[H]]): ChunkedDeser[H] = {
    ChunkedDeserOps.getChunkedInput.flatMap { chunkedInput =>
      val chunk = fieldGenerations.getOrElse(fieldName, 0: Byte)
      ChunkedDeserOps.recordFieldIndex(fieldName, chunk).flatMap { fieldPosition =>
//        println(s"Reading field $fieldName from chunk $chunk")
        if (chunkedInput.storedVersion < chunk) {
          // Field was not serialized
          fieldDefaults.get(fieldName) match {
            case Some(value) =>
              ChunkedDeserOps.pure(value.asInstanceOf[H])
            case None =>
              ChunkedDeserOps.failWith(DeserializationFailure(s"Field $fieldName is not in the stream and does not have default value", None))
          }
        } else {
          // Field was serialized

          if (chunkedInput.madeOptionalAt.contains(fieldPosition)) {
            // The field was made optional in by a newer version, reading as Option[H]
            for {
              input <- ChunkedDeserOps.fromEither(chunkedInput.inputFor(chunk))
              isDefined <- ChunkedDeserOps.fromDeser(booleanCodec.deserialize(), input)
              headValue <- if (isDefined) {
                ChunkedDeserOps.fromDeser(headCodec.value.deserialize(), input)
              } else {
                ChunkedDeserOps.failWith(NonOptionalFieldSerializedAsNone(fieldName))
              }
            } yield headValue
          } else {
            // Default case, reading the field from the given chunk
            for {
              input <- ChunkedDeserOps.fromEither(chunkedInput.inputFor(chunk))
              headValue <- ChunkedDeserOps.fromDeser(headCodec.value.deserialize(), input)
            } yield headValue
          }
        }
      }
    }
  }

  implicit def hlistOptionalDeserializer[K <: Symbol, H, T <: HList](implicit witness: Witness.Aux[K],
                                                                     headCodec: Lazy[BinaryCodec[H]],
                                                                     optHeadCodec: Lazy[BinaryCodec[Option[H]]],
                                                                     tailCodec: ChunkedBinaryDeserializer[T]): ChunkedBinaryDeserializer[FieldType[K, Option[H]] :: T] =
    () => {
      val fieldName = witness.value.name
      for {
        headValue <- readOptionalFieldIfExists[H](fieldName)
        tailValues <- tailCodec.deserialize()
      } yield field[K](headValue) :: tailValues
    }

  implicit def hlistDeserializer[K <: Symbol, H, T <: HList](implicit witness: Witness.Aux[K],
                                                             headCodec: Lazy[BinaryCodec[H]],
                                                             tailCodec: ChunkedBinaryDeserializer[T]): ChunkedBinaryDeserializer[FieldType[K, H] :: T] =
    () => {
      val fieldName = witness.value.name
      for {
        headValue <- readFieldIfExists(fieldName)
        tailValues <- tailCodec.deserialize()
      } yield field[K](headValue) :: tailValues
    }

  implicit def clistCodec[K <: Symbol, H, T <: Coproduct](implicit witness: Witness.Aux[K],
                                                          headCodec: Lazy[BinaryCodec[H]],
                                                          tailCodec: BinaryCodec[T]): BinaryCodec[FieldType[K, H] :+: T] = {
    ???
  }

  def derive[T, H](implicit gen: LabelledGeneric.Aux[T, H],
                   hlistSerializer: Lazy[ChunkedBinarySerializer[H]],
                   hlistDeserializer: Lazy[ChunkedBinaryDeserializer[H]]): BinaryCodec[T] =
    BinaryCodec.define[T] {
      value =>
        for {
          _ <- writeByte(version)
          primaryOutput <- getOutput
          chunkedOutput = createChunkedOutput(primaryOutput)
          hlist = gen.to(value)
          state <- getSerializerState
          initialState = ChunkedSerState(state, Map.empty, Map.empty)
          result <- Ser.fromEither(
            hlistSerializer.value.serialize(hlist)
              .run(chunkedOutput)
              .run(initialState))
          (finalState, _) = result
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
        initialState = ChunkedSerState(state, Map.empty, Map.empty)
        result <- Deser.fromEither(hlistDeserializer.value.deserialize()
          .run(chunkedInput)
          .run(initialState))
        (finalState, hlist) = result
        _ <- setDeserializerState(finalState.serializerState)
      } yield gen.from(hlist)
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
                  streams(v).flush();
                  streams(v).size()
                }
                Right(SerializedEvolutionStep.FieldAddedToNewChunk(size))
              case FieldAdded(_, _) =>
                val size = {
                  streams(v).flush();
                  streams(v).size()
                }
                Right(SerializedEvolutionStep.FieldAddedToNewChunk(size))
              case FieldMadeOptional(name) =>
                fieldIndices.get(name) match {
                  case Some(fieldPosition) =>
                    Right(SerializedEvolutionStep.FieldMadeOptional(fieldPosition))
                  case None =>
                    Left(UnknownFieldReferenceInEvolutionStep(name))
                }
              case FieldRemoved(name) =>
                // TODO
                Right(SerializedEvolutionStep.UnknownEvolutionStep)
              case FieldKeepReferences(name) =>
                // TODO
                Right(SerializedEvolutionStep.UnknownEvolutionStep)
              case _ =>
                Right(SerializedEvolutionStep.UnknownEvolutionStep)
            }
            serializedEvolutionStep match {
              case Left(failure) =>
                s >> failSerializerWith(failure)
              case Right(step) =>
                // println(s"Writing serialized evolution step $v: $step")
                s >> write[SerializedEvolutionStep](step)
            }
          }
        }

        override def writeOrderedChunks(): Ser[Unit] = {
          streams.foldLeft(finishSerializer()) {
            case (m, stream) => m >> writeBytes({
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
            // println(s"Reading $size bytes for chunk")
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

        override def inputFor(version: Byte): Either[DesertFailure, BinaryInput] =
          if (version < inputs.length) {
            Right(inputs(version))
          } else {
            Left(DeserializingNonExistingChunk(version))
          }
      }
    }
}

object GenericBinaryCodec {
  val simple = new GenericBinaryCodec(Vector(InitialVersion))

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

    case object UnknownEvolutionStep extends SerializedEvolutionStep

    implicit val codec: BinaryCodec[SerializedEvolutionStep] =
      BinaryCodec.define[SerializedEvolutionStep] {
        case FieldAddedToNewChunk(size) => writeVarInt(size, optimizeForPositive = false)
        case FieldMadeOptional(position) => writeVarInt(Codes.FieldMadeOptionalCode, optimizeForPositive = false) >> write(position)
        case UnknownEvolutionStep => writeVarInt(Codes.Unknown, optimizeForPositive = false)
      } {
        for {
          code <- readVarInt(optimizeForPositive = false)
          result <- code match {
            case Codes.Unknown => finishDeserializerWith(UnknownEvolutionStep)
            case Codes.FieldMadeOptionalCode => read[FieldPosition]().map(FieldMadeOptional.apply)
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
                             lastIndexPerChunk: Map[Byte, Byte],
                             fieldIndices: Map[String, FieldPosition])

  type ChunkedSer[T] = ReaderT[StateT[Either[DesertFailure, *], ChunkedSerState, *], ChunkedOutput, T]

  object ChunkedSerOps {
    final def fromEither[T](value: Either[DesertFailure, T]): ChunkedSer[T] = ReaderT.liftF(StateT.liftF(value))
    final def fromSer[T](value: Ser[T], output: BinaryOutput): ChunkedSer[T] =
      for {
        chunkedState <- ReaderT.liftF(StateT.get[Either[DesertFailure, *], ChunkedSerState])
        runResult <- fromEither(value.run(output).run(chunkedState.serializerState))
        (resultState, result) = runResult
        _ <- ReaderT.liftF(StateT.set[Either[DesertFailure, *], ChunkedSerState](chunkedState.copy(serializerState = resultState)))
      } yield result

    final def pure[T](value: T): ChunkedSer[T] = fromEither(Right[DesertFailure, T](value))

    final def unit: ChunkedSer[Unit] = pure(())

    final def getChunkedOutput: ChunkedSer[ChunkedOutput] = ReaderT.ask[StateT[Either[DesertFailure, *], ChunkedSerState, *], ChunkedOutput]

    final def recordFieldIndex(fieldName: String, chunk: Byte): ChunkedSer[Unit] =
      ReaderT.liftF {
        StateT.modify { state =>
          state.lastIndexPerChunk.get(chunk) match {
            case Some(lastIndex) =>
              val newIndex: Byte = (lastIndex + 1).toByte
              state.copy(
                lastIndexPerChunk = state.lastIndexPerChunk.updated(chunk, newIndex),
                fieldIndices = state.fieldIndices + (fieldName -> FieldPosition(chunk, newIndex))
              )
            case None =>
              state.copy(
                lastIndexPerChunk = state.lastIndexPerChunk + (chunk -> 0),
                fieldIndices = state.fieldIndices + (fieldName -> FieldPosition(chunk, 0))
              )
          }
        }
      }
  }

  trait ChunkedBinarySerializer[T] {
    def serialize(value: T): ChunkedSer[Unit]
  }

  trait ChunkedInput {
    val storedVersion: Byte
    val madeOptionalAt: Map[FieldPosition, Byte]

    def inputFor(version: Byte): Either[DesertFailure, BinaryInput]
  }

  type ChunkedDeser[T] = ReaderT[StateT[Either[DesertFailure, *], ChunkedSerState, *], ChunkedInput, T]

  object ChunkedDeserOps {
    final def fromEither[T](value: Either[DesertFailure, T]): ChunkedDeser[T] = ReaderT.liftF(StateT.liftF(value))
    final def fromDeser[T](value: Deser[T], input: BinaryInput): ChunkedDeser[T] =
      for {
        chunkedState <- ReaderT.liftF(StateT.get[Either[DesertFailure, *], ChunkedSerState])
        runResult <- fromEither(value.run(input).run(chunkedState.serializerState))
        (resultState, result) = runResult
        _ <- ReaderT.liftF(StateT.set[Either[DesertFailure, *], ChunkedSerState](chunkedState.copy(serializerState = resultState)))
      } yield result

    final def pure[T](value: T): ChunkedDeser[T] = fromEither(Right[DesertFailure, T](value))

    final def failWith[T](failure: DesertFailure): ChunkedDeser[T] = fromEither(Left(failure))

    final def getChunkedInput: ChunkedDeser[ChunkedInput] = ReaderT.ask[StateT[Either[DesertFailure, *], ChunkedSerState, *], ChunkedInput]

    final def recordFieldIndex(fieldName: String, chunk: Byte): ChunkedDeser[FieldPosition] =
      for {
        state <- ReaderT.liftF(StateT.get[Either[DesertFailure, *], ChunkedSerState])
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
        _ <- ReaderT.liftF(StateT.set[Either[DesertFailure, *], ChunkedSerState](newState))
      } yield position
  }

  trait ChunkedBinaryDeserializer[T] {
    def deserialize(): ChunkedDeser[T]
  }

}
