package io.github.vigoo.desert.custom

import io.github.vigoo.desert.internal.SerializerState.{RefAlreadyStored, RefIsNew, StoreRefResult, StoreStringResult}
import io.github.vigoo.desert.{BinaryCodec, BinarySerializer, DesertException, DesertFailure}

import java.util.zip.Deflater

trait BinarySerializerOps {

  final def failSerializerWith(failure: DesertFailure): Nothing =
    throw DesertException(failure)

  final def writeByte(value: Byte)(implicit ctx: SerializationContext): Unit =
    ctx.env.output.writeByte(value)

  final def writeShort(value: Short)(implicit ctx: SerializationContext): Unit =
    ctx.env.output.writeShort(value)

  final def writeInt(value: Int)(implicit ctx: SerializationContext): Unit =
    ctx.env.output.writeInt(value)

  final def writeVarInt(value: Int, optimizeForPositive: Boolean)(implicit ctx: SerializationContext): Unit =
    ctx.env.output.writeVarInt(value, optimizeForPositive)

  final def writeLong(value: Long)(implicit ctx: SerializationContext): Unit =
    ctx.env.output.writeLong(value)

  final def writeFloat(value: Float)(implicit ctx: SerializationContext): Unit =
    ctx.env.output.writeFloat(value)

  final def writeDouble(value: Double)(implicit ctx: SerializationContext): Unit =
    ctx.env.output.writeDouble(value)

  final def writeBytes(value: Array[Byte])(implicit ctx: SerializationContext): Unit =
    ctx.env.output.writeBytes(value)

  final def writeCompressedBytes(value: Array[Byte], level: Int = Deflater.BEST_SPEED)(implicit
      ctx: SerializationContext
  ): Unit =
    ctx.env.output.writeCompressedByteArray(value, level)

  final def writeUnknown(value: Any)(implicit ctx: SerializationContext): Unit =
    ctx.env.typeRegistry.get(value) match {
      case Some(registration) =>
        writeVarInt(registration.id.value, optimizeForPositive = true)
        registration.serialize(value)
      case None               =>
        failSerializerWith(DesertFailure.TypeNotRegistered(value.getClass))
    }

  final def write[U: BinarySerializer](value: U)(implicit ctx: SerializationContext): Unit =
    implicitly[BinarySerializer[U]].serialize(value)

  final def storeString(value: String)(implicit ctx: SerializationContext): StoreStringResult =
    ctx.state.storeString(value)

  final def storeRef(value: AnyRef)(implicit ctx: SerializationContext): StoreRefResult =
    ctx.state.storeRef(value)

  final def storeRefOrObject[T <: AnyRef](value: T)(implicit codec: BinaryCodec[T], ctx: SerializationContext): Unit =
    storeRef(value) match {
      case RefAlreadyStored(id) =>
        writeVarInt(id.value, optimizeForPositive = true)
      case RefIsNew(_)          =>
        writeVarInt(0, optimizeForPositive = true)
        write(value)
    }
}
