package io.github.vigoo.desert

import java.util.zip.Deflater

import io.github.vigoo.desert.BinaryDeserializer.Deser
import io.github.vigoo.desert.BinarySerializer.Ser
import io.github.vigoo.desert.SerializerState.{RefAlreadyStored, RefIsNew, StoreRefResult, StoreStringResult}
import shapeless.Lazy
import zio.prelude.fx._

import scala.util.Try

trait BinarySerializerOps {
  final def getOutput: Ser[BinaryOutput] = ZPure.access(_.output)
  final def getOutputTypeRegistry: Ser[TypeRegistry] = ZPure.access(_.typeRegistry)
  final def getSerializerState: Ser[SerializerState] = ZPure.get
  final def setSerializerState(state: SerializerState): Ser[Unit] = ZPure.set(state)

  final def writeByte(value: Byte): Ser[Unit] = getOutput.flatMap(output => Ser.fromEither(output.writeByte(value)))
  final def writeShort(value: Short): Ser[Unit] = getOutput.flatMap(output => Ser.fromEither(output.writeShort(value)))
  final def writeInt(value: Int): Ser[Unit] = getOutput.flatMap(output => Ser.fromEither(output.writeInt(value)))
  final def writeVarInt(value: Int, optimizeForPositive: Boolean): Ser[Unit] = getOutput.flatMap(output => Ser.fromEither(output.writeVarInt(value, optimizeForPositive)))
  final def writeLong(value: Long): Ser[Unit] = getOutput.flatMap(output => Ser.fromEither(output.writeLong(value)))
  final def writeFloat(value: Float): Ser[Unit] = getOutput.flatMap(output => Ser.fromEither(output.writeFloat(value)))
  final def writeDouble(value: Double): Ser[Unit] = getOutput.flatMap(output => Ser.fromEither(output.writeDouble(value)))
  final def writeBytes(value: Array[Byte]): Ser[Unit] = getOutput.flatMap(output => Ser.fromEither(output.writeBytes(value)))
  final def writeCompressedBytes(value: Array[Byte], level: Int = Deflater.BEST_SPEED): Ser[Unit] = getOutput.flatMap(output => Ser.fromEither(output.writeCompressedByteArray(value, level)))

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
  final def serializerFromTry[T](f: Try[T], failMessage: String): Deser[T] = Deser.fromEither(f.toEither.left.map(failure => SerializationFailure(failMessage, Some(failure))))

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
      case RefIsNew(_) => writeVarInt(0, optimizeForPositive = true) *> write(value)(codec.value)
    }
}

object BinarySerializerOps extends BinarySerializerOps
