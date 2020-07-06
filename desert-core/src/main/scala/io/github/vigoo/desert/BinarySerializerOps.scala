package io.github.vigoo.desert

import cats.data.{ReaderT, StateT}
import cats.instances.either._
import cats.syntax.flatMap._
import io.github.vigoo.desert.BinarySerializer.{Ser, SerializationEnv}
import io.github.vigoo.desert.SerializerState.{RefAlreadyStored, RefIsNew, StoreRefResult, StoreStringResult}
import shapeless.Lazy

trait BinarySerializerOps {
  final def getOutput: Ser[BinaryOutput] = ReaderT.ask[StateT[Either[DesertFailure, *], SerializerState, *], SerializationEnv].map(_.output)
  final def getOutputTypeRegistry: Ser[TypeRegistry] = ReaderT.ask[StateT[Either[DesertFailure, *], SerializerState, *], SerializationEnv].map(_.typeRegistry)
  final def getSerializerState: Ser[SerializerState] = ReaderT.liftF(StateT.get[Either[DesertFailure, *], SerializerState])
  final def setSerializerState(state: SerializerState): Ser[Unit] = ReaderT.liftF(StateT.set[Either[DesertFailure, *], SerializerState](state))

  final def writeByte(value: Byte): Ser[Unit] = getOutput.flatMap(output => Ser.fromEither(output.writeByte(value)))
  final def writeShort(value: Short): Ser[Unit] = getOutput.flatMap(output => Ser.fromEither(output.writeShort(value)))
  final def writeInt(value: Int): Ser[Unit] = getOutput.flatMap(output => Ser.fromEither(output.writeInt(value)))
  final def writeVarInt(value: Int, optimizeForPositive: Boolean): Ser[Unit] = getOutput.flatMap(output => Ser.fromEither(output.writeVarInt(value, optimizeForPositive)))
  final def writeLong(value: Long): Ser[Unit] = getOutput.flatMap(output => Ser.fromEither(output.writeLong(value)))
  final def writeFloat(value: Float): Ser[Unit] = getOutput.flatMap(output => Ser.fromEither(output.writeFloat(value)))
  final def writeDouble(value: Double): Ser[Unit] = getOutput.flatMap(output => Ser.fromEither(output.writeDouble(value)))
  final def writeBytes(value: Array[Byte]): Ser[Unit] = getOutput.flatMap(output => Ser.fromEither(output.writeBytes(value)))

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
  final def finishSerializerWith[T](value: T): Ser[T] = Ser.fromEither(Right(value))
  final def failSerializerWith(failure: DesertFailure): Ser[Unit] = Ser.fromEither(Left(failure))

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
      case RefIsNew(_) => writeVarInt(0, optimizeForPositive = true) >> write(value)(codec.value)
    }
}

object BinarySerializerOps extends BinarySerializerOps
