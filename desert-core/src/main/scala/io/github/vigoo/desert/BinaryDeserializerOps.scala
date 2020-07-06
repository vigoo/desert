package io.github.vigoo.desert

import io.github.vigoo.desert.BinaryDeserializer.{Deser, DeserializationEnv}
import io.github.vigoo.desert.SerializerState.{RefId, StringId}
import io.github.vigoo.desert.TypeRegistry.RegisteredTypeId
import shapeless.Lazy
import zio.ZIO

trait BinaryDeserializerOps {
  final def getInput: Deser[BinaryInput] = ZIO.environment[DeserializationEnv].map(_.input)
  final def getInputTypeRegistry: Deser[TypeRegistry] = ZIO.environment[DeserializationEnv].map(_.typeRegistry)
  final def getDeserializerState: Deser[SerializerState] = ZIO.environment[DeserializationEnv].flatMap(_.state.get)
  final def setDeserializerState(state: SerializerState): Deser[Unit] = ZIO.environment[DeserializationEnv].flatMap(_.state.set(state))

  final def readByte(): Deser[Byte] = getInput.flatMap(_.readByte())
  final def readShort(): Deser[Short] = getInput.flatMap(_.readShort())
  final def readInt(): Deser[Int] = getInput.flatMap(_.readInt())
  final def readVarInt(optimizeForPositive: Boolean): Deser[Int] = getInput.flatMap(_.readVarInt(optimizeForPositive))
  final def readLong(): Deser[Long] = getInput.flatMap(_.readLong())
  final def readFloat(): Deser[Float] = getInput.flatMap(_.readFloat())
  final def readDouble(): Deser[Double] = getInput.flatMap(_.readDouble())
  final def readBytes(count: Int): Deser[Array[Byte]] = getInput.flatMap(_.readBytes(count))
  final def read[T: BinaryDeserializer](): Deser[T] = implicitly[BinaryDeserializer[T]].deserialize()

  final def readUnknown(): Deser[Any] =
    for {
      typeRegistry <- getInputTypeRegistry
      typeId <- readVarInt(optimizeForPositive = true).map(RegisteredTypeId)
      result <- typeRegistry.forId(typeId) match {
        case Some(registration) =>
          registration.codec.deserialize()
        case None =>
          failDeserializerWith(InvalidTypeId(typeId))
      }
    } yield result

  final def finishDeserializerWith[T](value: T): Deser[T] = ZIO.succeed(value)
  final def failDeserializerWith[T](failure: DesertFailure): Deser[T] = ZIO.fail(failure)

  final def getString(value: StringId): Deser[Option[String]] =
    for {
      state <- getDeserializerState
    } yield state.stringsById.get(value)

  final def storeReadString(value: String): Deser[Unit] =
    for {
      state <- getDeserializerState
      (newState, _) = state.storeString(value)
      _ <- setDeserializerState(newState)
    } yield ()

  final def getRef(value: RefId): Deser[Option[AnyRef]] =
    for {
      state <- getDeserializerState
    } yield state.refsById.get(value)

  final def storeReadRef(value: AnyRef): Deser[Unit] =
    for {
      state <- getDeserializerState
      (newState, _) = state.storeRef(value)
      _ <- setDeserializerState(newState)
    } yield ()

  def readRefOrValue[T <: AnyRef](storeReadReference: Boolean = true)(implicit codec: Lazy[BinaryCodec[T]]): Deser[T] =
    readVarInt(optimizeForPositive = true).flatMap {
      case 0 => for {
        value <- read[T]()(codec.value)
        _ <- if (storeReadReference) storeReadRef(value) else finishDeserializerWith(())
      } yield value
      case id => getRef(RefId(id)).flatMap {
        case None => failDeserializerWith(InvalidRefId(RefId(id)))
        case Some(value) => finishDeserializerWith(value.asInstanceOf[T])
      }
    }
}

object BinaryDeserializerOps extends BinaryDeserializerOps
