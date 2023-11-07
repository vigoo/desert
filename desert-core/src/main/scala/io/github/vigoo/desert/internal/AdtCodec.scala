package io.github.vigoo.desert.internal

import io.github.vigoo.desert.{custom, _}
import io.github.vigoo.desert.Evolution._
import io.github.vigoo.desert.custom._

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import scala.annotation.nowarn
import scala.collection.mutable

class AdtCodec[T, BuilderState](
    evolutionSteps: Vector[Evolution],
    typeName: String,
    constructors: Vector[String],
    transientFields: Map[String, Any],
    getSerializationCommands: T => List[AdtCodec.SerializationCommand],
    deserializationCommands: List[AdtCodec.DeserializationCommand[BuilderState]],
    initialBuilderState: () => BuilderState,
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

  override def serialize(value: T)(implicit ctx: SerializationContext): Unit = {
    writeByte(version)
    val primaryOutput                                    = ctx.env.output
    val chunkedOutput                                    = createChunkedOutput(primaryOutput)
    val typeRegistry                                     = ctx.env.typeRegistry
    val chunkedState                                     = new ChunkedSerState(
      ctx.state,
      typeRegistry,
      lastIndexPerChunk = mutable.Map.empty,
      fieldIndices = mutable.Map.empty,
      constructorNameToId,
      constructorIdToName,
      typeDescription = typeName,
      readConstructorName = None,
      transientFields
    )
    implicit val chunkedCtx: ChunkedSerializationContext = ChunkedSerializationContext(chunkedState, chunkedOutput)
    val commands                                         = getSerializationCommands(value)
    for (command <- commands)
      serialize(command)
    chunkedOutput.writeEvolutionHeader(chunkedState.fieldIndices)
    chunkedOutput.writeOrderedChunks()
  }

  override def deserialize()(implicit ctx: DeserializationContext): T = {
    val storedVersion                                      = readByte()
    val primaryInput                                       = ctx.env.input
    val chunkedInput                                       = createChunkedInput(primaryInput, storedVersion)
    val typeRegistry                                       = ctx.env.typeRegistry
    val chunkedState                                       = new ChunkedSerState(
      ctx.state,
      typeRegistry,
      lastIndexPerChunk = mutable.Map.empty,
      fieldIndices = mutable.Map.empty,
      constructorNameToId,
      constructorIdToName,
      typeDescription = typeName,
      readConstructorName = None,
      transientFields
    )
    implicit val chunkedCtx: ChunkedDeserializationContext = ChunkedDeserializationContext(chunkedState, chunkedInput)
    val finalBuilderState                                  = deserializationCommands.foldLeft(initialBuilderState())(deserialize)

    materialize(finalBuilderState) match {
      case Left(failure) => throw DesertException(failure)
      case Right(value)  => value
    }
  }

  private def createChunkedOutput(primaryOutput: BinaryOutput): ChunkedOutput =
    if (version == 0) {
      // Simple mode: we serialize directly to the main stream
      new ChunkedOutput {
        override def outputFor(version: Byte): BinaryOutput = primaryOutput

        override def writeEvolutionHeader(fieldIndices: mutable.Map[String, FieldPosition])(implicit
            ctx: SerializationContext
        ): Unit = {}

        override def writeOrderedChunks()(implicit ctx: SerializationContext): Unit = {}
      }
    } else {
      new ChunkedOutput {
        private val streams: Array[ByteArrayOutputStream]  = (0 to version).map(_ => new ByteArrayOutputStream()).toArray
        private val outputs: Array[JavaStreamBinaryOutput] = streams.map(new JavaStreamBinaryOutput(_))

        override def outputFor(version: Byte): BinaryOutput = outputs(version)

        override def writeEvolutionHeader(
            fieldIndices: mutable.Map[String, FieldPosition]
        )(implicit ctx: SerializationContext): Unit =
          for (v <- 0 to version) {
            val serializedEvolutionStep: SerializedEvolutionStep =
              evolutionSteps(v) match {
                case InitialVersion          =>
                  val size = {
                    streams(v).flush()
                    streams(v).size()
                  }
                  SerializedEvolutionStep.FieldAddedToNewChunk(size)
                case FieldAdded(_, _)        =>
                  val size = {
                    streams(v).flush()
                    streams(v).size()
                  }
                  SerializedEvolutionStep.FieldAddedToNewChunk(size)
                case FieldMadeOptional(name) =>
                  fieldIndices.get(name) match {
                    case Some(fieldPosition) =>
                      SerializedEvolutionStep.FieldMadeOptional(fieldPosition)
                    case None                =>
                      if (removedFields.contains(name)) {
                        SerializedEvolutionStep.FieldMadeOptional(FieldPosition.removed)
                      } else {
                        throw DesertException(DesertFailure.UnknownFieldReferenceInEvolutionStep(name))
                      }
                  }
                case FieldRemoved(name)      =>
                  SerializedEvolutionStep.FieldRemoved(name)
                case _                       =>
                  SerializedEvolutionStep.UnknownEvolutionStep
              }
            write(serializedEvolutionStep)
          }

        override def writeOrderedChunks()(implicit ctx: SerializationContext): Unit =
          for (stream <- streams) {
            stream.flush()
            writeBytes(stream.toByteArray)
          }
      }
    }

  private def createChunkedInput(primaryInput: BinaryInput, storedVer: Byte)(implicit
      ctx: DeserializationContext
  ): ChunkedInput =
    if (storedVer == 0) {
      // Simple mode: deserializing directly from the input stream
      new ChunkedInput {
        override val storedVersion: Byte = storedVer

        override val madeOptionalAt: Map[FieldPosition, Byte] = Map.empty

        override val removedFields: Set[String] = Set.empty

        override def inputFor(version: Byte): BinaryInput =
          if (version == 0) primaryInput
          else throw DesertException(DesertFailure.DeserializingNonExistingChunk(version))
      }
    } else {
      val serializedEvolutionSteps = new Array[SerializedEvolutionStep](storedVer + 1)
      for (v <- 0 to storedVer)
        serializedEvolutionSteps(v) = read[SerializedEvolutionStep]()
      val chunks = new Array[Array[Byte]](serializedEvolutionSteps.length)
      for (v <- 0 to storedVer)
        serializedEvolutionSteps(v) match {
          case SerializedEvolutionStep.FieldAddedToNewChunk(size) =>
            chunks(v) = readBytes(size)
          case _                                                  =>
            chunks(v) = Array.empty[Byte]
        }
      new ChunkedInput {
        private val streams = chunks.map(new ByteArrayInputStream(_))
        private val inputs  = streams.map(new JavaStreamBinaryInput(_))

        override val storedVersion: Byte = storedVer

        override val madeOptionalAt: Map[FieldPosition, Byte] =
          serializedEvolutionSteps.zipWithIndex.collect {
            case (SerializedEvolutionStep.FieldMadeOptional(position), idx) => (position, idx.toByte)
          }.toMap

        override val removedFields: Set[String] =
          serializedEvolutionSteps.collect { case SerializedEvolutionStep.FieldRemoved(name) => name }.toSet

        override def inputFor(version: Byte): BinaryInput =
          if (version < inputs.length) {
            inputs(version)
          } else {
            throw DesertException(DesertFailure.DeserializingNonExistingChunk(version))
          }
      }
    }

  private def readOptionalFieldIfExists[H](
      fieldName: String,
      headCodec: BinaryCodec[H],
      optHeadCodec: BinaryCodec[Option[H]]
  )(implicit ctx: ChunkedDeserializationContext): Option[H] =
    if (ctx.input.removedFields.contains(fieldName)) {
      None
    } else {
      val chunk    = fieldGenerations.getOrElse(fieldName, 0: Byte)
      val optSince = madeOptionalAt.getOrElse(fieldName, 0: Byte)

      ChunkedDeserOps.recordFieldIndex(fieldName, chunk)
      if (ctx.input.storedVersion < chunk) {
        // This field was not serialized
        fieldDefaults.get(fieldName) match {
          case Some(value) =>
            if (optSince <= chunk) {
              // It was originally Option[H]
              value.asInstanceOf[Option[H]]
            } else {
              // It was made optional after it was added
              Some(value.asInstanceOf[H])
            }
          case None        =>
            throw DesertException(
              DesertFailure.DeserializationFailure(
                s"Field $fieldName is not in the stream and does not have default value",
                None
              )
            )
        }
      } else {
        val input                                 = ctx.input.inputFor(chunk)
        implicit val dctx: DeserializationContext = ChunkedDeserOps.toDeserializationContext(input)

        // This field was serialized
        if (ctx.input.storedVersion < optSince) {
          // Expect H in the input stream and wrap with Some()
          Some(headCodec.deserialize())
        } else {
          // Expect Option[H] in the input stream
          optHeadCodec.deserialize()
        }
      }
    }

  private def readFieldIfExists[H](fieldName: String, headCodec: BinaryCodec[H])(implicit
      ctx: ChunkedDeserializationContext
  ): H =
    if (ctx.input.removedFields.contains(fieldName))
      throw DesertException(DesertFailure.FieldRemovedInSerializedVersion(fieldName))
    else {
      val chunk         = fieldGenerations.getOrElse(fieldName, 0: Byte)
      val fieldPosition = ChunkedDeserOps.recordFieldIndex(fieldName, chunk)
      if (ctx.input.storedVersion < chunk) {
        // Field was not serialized
        fieldDefaults.get(fieldName) match {
          case Some(value) =>
            value.asInstanceOf[H]
          case None        =>
            throw DesertException(DesertFailure.FieldWithoutDefaultValueIsMissing(fieldName))
        }
      } else {
        // Field was serialized
        val input                                 = ctx.input.inputFor(chunk)
        implicit val dctx: DeserializationContext = ChunkedDeserOps.toDeserializationContext(input)

        if (ctx.input.madeOptionalAt.contains(fieldPosition)) {
          // The field was made optional in by a newer version, reading as Option[H]

          val isDefined = read[Boolean]()
          if (isDefined)
            headCodec.deserialize()
          else
            throw DesertException(DesertFailure.NonOptionalFieldSerializedAsNone(fieldName))
        } else {
          // Default case, reading the field from the given chunk
          headCodec.deserialize()
        }
      }
    }

  private def serialize(command: SerializationCommand)(implicit ctx: ChunkedSerializationContext): Unit =
    command match {
      case SerializationCommand.WriteField(fieldName, value, codec)             =>
        val chunk                               = fieldGenerations.getOrElse(fieldName, 0: Byte)
        val output                              = ctx.output.outputFor(chunk)
        implicit val sctx: SerializationContext = ChunkedSerOps.getSerializationContext(output)
        codec().serialize(value)
        ChunkedSerOps.recordFieldIndex(fieldName, chunk)
      case SerializationCommand.WriteConstructor(constructorName, value, codec) =>
        val output                              = ctx.output.outputFor(0)
        implicit val sctx: SerializationContext = ChunkedSerOps.getSerializationContext(output)
        val constructorId                       = ChunkedSerOps.getConstructorId(constructorName)
        writeVarInt(constructorId, optimizeForPositive = true)
        codec().serialize(value)
      case SerializationCommand.Fail(failure)                                   =>
        throw DesertException(failure)
    }

  @nowarn("msg=.*eliminated by erasure") private def deserialize(
      builderState: BuilderState,
      command: DeserializationCommand[BuilderState]
  )(implicit ctx: ChunkedDeserializationContext): BuilderState =
    command match {
      case DeserializationCommand.Read(fieldName, codec, store) =>
        store(readFieldIfExists(fieldName, codec()), builderState)

      case DeserializationCommand.ReadOptional(fieldName, codec, optCodec, store) =>
        store(readOptionalFieldIfExists(fieldName, codec(), optCodec()), builderState)

      case readTransient: DeserializationCommand.ReadTransient[Any, BuilderState] =>
        val value =
          ctx.state.getTransient(readTransient.fieldName) match {
            case Some(value) => value
            case None        =>
              throw DesertException(
                DesertFailure.DeserializationFailure(
                  s"Illegal state while processing transient field ${readTransient.fieldName}",
                  None
                )
              )
          }
        readTransient.store(value, builderState)

      case DeserializationCommand.ReadConstructor(expectedConstructorName, codec, store) =>
        val input           = ctx.input.inputFor(0)
        val constructorName = ChunkedDeserOps.readOrGetConstructorName(input)
        if (expectedConstructorName == constructorName) {
          implicit val dctx: DeserializationContext = ChunkedDeserOps.toDeserializationContext(input)
          store(codec().deserialize(), builderState)
        } else {
          builderState
        }
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
      new BinaryCodec[SerializedEvolutionStep] {
        override def deserialize()(implicit ctx: DeserializationContext): SerializedEvolutionStep = {
          val code = readVarInt(optimizeForPositive = false)
          code match {
            case Codes.Unknown               => UnknownEvolutionStep
            case Codes.FieldMadeOptionalCode => FieldMadeOptional(read[FieldPosition]())
            case Codes.FieldRemovedCode      => FieldRemoved(read[DeduplicatedString]().string)
            case size if size > 0            => FieldAddedToNewChunk(size)
            case _                           => failDeserializerWith(DesertFailure.UnknownSerializedEvolutionStep(code))
          }
        }

        override def serialize(value: SerializedEvolutionStep)(implicit context: SerializationContext): Unit =
          value match {
            case FieldAddedToNewChunk(size)  =>
              writeVarInt(size, optimizeForPositive = false)
            case FieldMadeOptional(position) =>
              writeVarInt(Codes.FieldMadeOptionalCode, optimizeForPositive = false)
              write(position)
            case FieldRemoved(fieldName)     =>
              writeVarInt(Codes.FieldRemovedCode, optimizeForPositive = false)
              write(DeduplicatedString(fieldName))
            case UnknownEvolutionStep        =>
              writeVarInt(Codes.Unknown, optimizeForPositive = false)
          }
      }
  }

  trait ChunkedOutput {
    def outputFor(version: Byte): BinaryOutput

    def writeEvolutionHeader(fieldIndices: mutable.Map[String, FieldPosition])(implicit ctx: SerializationContext): Unit

    def writeOrderedChunks()(implicit ctx: SerializationContext): Unit
  }

  final class ChunkedSerState(
      val serializerState: SerializerState,
      val typeRegistry: TypeRegistry,
      lastIndexPerChunk: mutable.Map[Byte, Byte],
      val fieldIndices: mutable.Map[String, FieldPosition],
      constructorNameToId: Map[String, Int],
      constructorIdToName: Map[Int, String],
      typeDescription: String,
      var readConstructorName: Option[String],
      transientFields: Map[String, Any]
  ) {
    def getConstructorId(typeName: String): Int =
      constructorNameToId.get(typeName) match {
        case Some(value) => value
        case None        => throw DesertException(DesertFailure.InvalidConstructorName(typeName, typeDescription))
      }

    def getConstructorName(id: Int): String =
      constructorIdToName.get(id) match {
        case Some(value) => value
        case None        => throw DesertException(DesertFailure.InvalidConstructorId(id, typeDescription))
      }

    def getLastIndexPerChunk(chunk: Byte): Option[Byte] =
      lastIndexPerChunk.get(chunk)

    def updateLastIndex(chunk: Byte, newIndex: Byte): Unit =
      lastIndexPerChunk.update(chunk, newIndex)

    def addIndex(fieldName: String, position: FieldPosition): Unit =
      fieldIndices.update(fieldName, position)

    def getTransient(fieldName: String): Option[Any] =
      transientFields.get(fieldName)
  }

  final case class ChunkedSerializationContext(state: ChunkedSerState, output: ChunkedOutput)

  object ChunkedSerOps {
    final def getSerializationContext(output: BinaryOutput)(implicit
        ctx: ChunkedSerializationContext
    ): SerializationContext =
      custom.SerializationContext(SerializationEnv(output, ctx.state.typeRegistry), ctx.state.serializerState)

    final def recordFieldIndex(fieldName: String, chunk: Byte)(implicit ctx: ChunkedSerializationContext): Unit =
      ctx.state.getLastIndexPerChunk(chunk) match {
        case Some(lastIndex) =>
          val newIndex: Byte = (lastIndex + 1).toByte
          ctx.state.updateLastIndex(chunk, newIndex)
          ctx.state.addIndex(fieldName, FieldPosition(chunk, newIndex))
        case None            =>
          ctx.state.updateLastIndex(chunk, 0)
          ctx.state.addIndex(fieldName, FieldPosition(chunk, 0))
      }

    final def getConstructorId(typeName: String)(implicit ctx: ChunkedSerializationContext): Int =
      ctx.state.getConstructorId(typeName)
  }

  trait ChunkedInput {
    val storedVersion: Byte
    val madeOptionalAt: Map[FieldPosition, Byte]
    val removedFields: Set[String]

    def inputFor(version: Byte): BinaryInput
  }

  final case class ChunkedDeserializationContext(state: ChunkedSerState, input: ChunkedInput)

  object ChunkedDeserOps {
    final def toDeserializationContext(input: BinaryInput)(implicit
        ctx: ChunkedDeserializationContext
    ): DeserializationContext =
      custom.DeserializationContext(DeserializationEnv(input, ctx.state.typeRegistry), ctx.state.serializerState)

    final def recordFieldIndex(fieldName: String, chunk: Byte)(implicit
        ctx: ChunkedDeserializationContext
    ): FieldPosition =
      ctx.state.getLastIndexPerChunk(chunk) match {
        case Some(lastIndex) =>
          val newIndex: Byte = (lastIndex + 1).toByte
          val fp             = FieldPosition(chunk, newIndex)
          ctx.state.updateLastIndex(chunk, newIndex)
          ctx.state.addIndex(fieldName, fp)
          fp
        case None            =>
          val fp = FieldPosition(chunk, 0)
          ctx.state.updateLastIndex(chunk, 0)
          ctx.state.addIndex(fieldName, fp)
          fp
      }

    final def getConstructorName(id: Int)(implicit ctx: ChunkedDeserializationContext): String =
      ctx.state.getConstructorName(id)

    final def readOrGetConstructorName(input: BinaryInput)(implicit ctx: ChunkedDeserializationContext): String =
      ctx.state.readConstructorName match {
        case Some(value) => value
        case None        =>
          implicit val dctx: DeserializationContext = toDeserializationContext(input)
          val constructorId                         = readVarInt(optimizeForPositive = true)
          val constructorName                       = getConstructorName(constructorId)
          ctx.state.readConstructorName = Some(constructorName)
          constructorName
      }
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
