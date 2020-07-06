package io.github.vigoo.desert

import io.github.vigoo.desert.BinarySerializer.{Ser, SerializationEnv}
import io.github.vigoo.desert.SerializerState.{RefAlreadyStored, RefIsNew, StoreRefResult, StoreStringResult}
import shapeless.Lazy
import zio.ZIO

trait BinarySerializerOps {
  final def getOutput: Ser[BinaryOutput] = ZIO.environment[SerializationEnv].map(_.output)
  final def getOutputTypeRegistry: Ser[TypeRegistry] = ZIO.environment[SerializationEnv].map(_.typeRegistry)
  final def getSerializerState: Ser[SerializerState] = ZIO.environment[SerializationEnv].flatMap(_.state.get)
  final def setSerializerState(state: SerializerState): Ser[Unit] = ZIO.environment[SerializationEnv].flatMap(_.state.set(state))

  final def writeByte(value: Byte): Ser[Unit] = getOutput.flatMap(_.writeByte(value))
  final def writeShort(value: Short): Ser[Unit] = getOutput.flatMap(_.writeShort(value))
  final def writeInt(value: Int): Ser[Unit] = getOutput.flatMap(_.writeInt(value))
  final def writeVarInt(value: Int, optimizeForPositive: Boolean): Ser[Unit] = getOutput.flatMap(_.writeVarInt(value, optimizeForPositive))
  final def writeLong(value: Long): Ser[Unit] = getOutput.flatMap(_.writeLong(value))
  final def writeFloat(value: Float): Ser[Unit] = getOutput.flatMap(_.writeFloat(value))
  final def writeDouble(value: Double): Ser[Unit] = getOutput.flatMap(_.writeDouble(value))
  final def writeBytes(value: Array[Byte]): Ser[Unit] = getOutput.flatMap(_.writeBytes(value))

  final def writeUnknown(value: Any): Ser[Unit] =
    getOutputTypeRegistry.flatMap { typeRegistry =>
      typeRegistry.get(value) match {
        case Some(registration) =>
          for {
            _ <- writeVarInt(registration.id.value, optimizeForPositive = true)
            _ <- registration.serialize(value)
          } yield ()
        case None =>
          failSerializerWith(TypeNotRegistered(value.getClass))
      }
    }

  final def write[U : BinarySerializer](value: U): Ser[Unit] = implicitly[BinarySerializer[U]].serialize(value)

  final def finishSerializer(): Ser[Unit] = finishSerializerWith(())
  final def finishSerializerWith[T](value: T): Ser[T] = ZIO.succeed(value)
  final def failSerializerWith(failure: DesertFailure): Ser[Unit] = ZIO.fail(failure)

  final def storeString(value: String): Ser[StoreStringResult] =
    for {
      state <- getSerializerState
      (newState, result) = state.storeString(value)
      _ <- setSerializerState(newState)
    } yield result

  final def storeRef(value: AnyRef): Ser[StoreRefResult] =
    for {
      state <- getSerializerState
      (newState, result) = state.storeRef(value)
      _ <- setSerializerState(newState)
    } yield result

  final def storeRefOrObject[T <: AnyRef](value: T)(implicit codec: Lazy[BinaryCodec[T]]): Ser[Unit] =
    storeRef(value).flatMap {
      case RefAlreadyStored(id) => writeVarInt(id.value, optimizeForPositive = true)
      case RefIsNew(_) => for {
        _ <- writeVarInt(0, optimizeForPositive = true)
        _ <- write(value)(codec.value)
      } yield ()
    }
}

object BinarySerializerOps extends BinarySerializerOps
