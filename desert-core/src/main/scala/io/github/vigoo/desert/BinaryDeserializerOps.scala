package io.github.vigoo.desert

import io.github.vigoo.desert.BinaryDeserializer.Deser
import io.github.vigoo.desert.SerializerState.{RefId, StringId}
import io.github.vigoo.desert.TypeRegistry.RegisteredTypeId
import _root_.zio.prelude.fx._

import scala.util.Try

trait BinaryDeserializerOps {
  final def getInput: Deser[BinaryInput]                              = ZPure.serviceWith(_.input)
  final def getInputTypeRegistry: Deser[TypeRegistry]                 = ZPure.serviceWith(_.typeRegistry)
  final def getDeserializerState: Deser[SerializerState]              = ZPure.get
  final def setDeserializerState(state: SerializerState): Deser[Unit] = ZPure.set(state)

  final def readByte(): Deser[Byte]                              = getInput.flatMap(input => Deser.fromEither(input.readByte()))
  final def readShort(): Deser[Short]                            = getInput.flatMap(input => Deser.fromEither(input.readShort()))
  final def readInt(): Deser[Int]                                = getInput.flatMap(input => Deser.fromEither(input.readInt()))
  final def readVarInt(optimizeForPositive: Boolean): Deser[Int] =
    getInput.flatMap(input => Deser.fromEither(input.readVarInt(optimizeForPositive)))
  final def readLong(): Deser[Long]                              = getInput.flatMap(input => Deser.fromEither(input.readLong()))
  final def readFloat(): Deser[Float]                            = getInput.flatMap(input => Deser.fromEither(input.readFloat()))
  final def readDouble(): Deser[Double]                          = getInput.flatMap(input => Deser.fromEither(input.readDouble()))
  final def readBytes(count: Int): Deser[Array[Byte]]            =
    getInput.flatMap(input => Deser.fromEither(input.readBytes(count)))
  final def readCompressedByteArray(): Deser[Array[Byte]]        =
    getInput.flatMap(input => Deser.fromEither(input.readCompressedByteArray()))
  final def read[T: BinaryDeserializer](): Deser[T]              = implicitly[BinaryDeserializer[T]].deserialize()

  final def readUnknown(): Deser[Any] =
    for {
      typeRegistry <- getInputTypeRegistry
      typeId       <- readVarInt(optimizeForPositive = true).map(RegisteredTypeId)
      result       <- typeRegistry.forId(typeId) match {
                        case Some(registration) =>
                          registration.codec.deserialize()
                        case None               =>
                          failDeserializerWith(InvalidTypeId(typeId))
                      }
    } yield result

  final def finishDeserializerWith[T](value: T): Deser[T]                    = Deser.fromEither(Right(value))
  final def failDeserializerWith[T](failure: DesertFailure): Deser[T]        = Deser.fromEither(Left(failure))
  final def deserializerFromTry[T](f: Try[T], failMessage: String): Deser[T] =
    Deser.fromEither(f.toEither.left.map(failure => DeserializationFailure(failMessage, Some(failure))))

  final def getString(value: StringId): Deser[Option[String]] =
    for {
      state <- getDeserializerState
    } yield state.stringsById.get(value)

  final def storeReadString(value: String): Deser[Unit] =
    for {
      state        <- getDeserializerState
      (newState, _) = state.storeString(value)
      _            <- setDeserializerState(newState)
    } yield ()

  final def getRef(value: RefId): Deser[Option[AnyRef]] =
    for {
      state <- getDeserializerState
    } yield state.refsById.get(value)

  final def storeReadRef(value: AnyRef): Deser[Unit] =
    for {
      state        <- getDeserializerState
      (newState, _) = state.storeRef(value)
      _            <- setDeserializerState(newState)
    } yield ()

  // TODO: need the codec to be lazy?
  def readRefOrValue[T <: AnyRef](storeReadReference: Boolean = true)(implicit codec: BinaryCodec[T]): Deser[T] =
    readVarInt(optimizeForPositive = true).flatMap {
      case 0  =>
        for {
          value <- read[T]()
          _     <- if (storeReadReference) storeReadRef(value) else finishDeserializerWith(())
        } yield value
      case id =>
        getRef(RefId(id)).flatMap {
          case None        => failDeserializerWith(InvalidRefId(RefId(id)))
          case Some(value) => finishDeserializerWith(value.asInstanceOf[T])
        }
    }
}

object BinaryDeserializerOps extends BinaryDeserializerOps
