package io.github.vigoo.desert.custom

import io.github.vigoo.desert.TypeRegistry.RegisteredTypeId
import io.github.vigoo.desert.internal.SerializerState.{RefId, StringId}
import io.github.vigoo.desert.{BinaryCodec, BinaryDeserializer, DesertException, DesertFailure}

trait BinaryDeserializerOps {

  final def failDeserializerWith[T](failure: DesertFailure): Nothing = throw DesertException(failure)

  final def readByte()(implicit ctx: DeserializationContext): Byte = ctx.env.input.readByte()

  final def readShort()(implicit ctx: DeserializationContext): Short = ctx.env.input.readShort()

  final def readInt()(implicit ctx: DeserializationContext): Int = ctx.env.input.readInt()

  final def readVarInt(optimizeForPositive: Boolean)(implicit ctx: DeserializationContext): Int =
    ctx.env.input.readVarInt(optimizeForPositive)

  final def readLong()(implicit ctx: DeserializationContext): Long = ctx.env.input.readLong()

  final def readFloat()(implicit ctx: DeserializationContext): Float = ctx.env.input.readFloat()

  final def readDouble()(implicit ctx: DeserializationContext): Double = ctx.env.input.readDouble()

  final def readBytes(count: Int)(implicit ctx: DeserializationContext): Array[Byte] =
    ctx.env.input.readBytes(count)

  final def readCompressedByteArray()(implicit ctx: DeserializationContext): Array[Byte] =
    ctx.env.input.readCompressedByteArray()

  final def read[T: BinaryDeserializer]()(implicit ctx: DeserializationContext): T =
    implicitly[BinaryDeserializer[T]].deserialize()

  final def readUnknown()(implicit ctx: DeserializationContext): Any = {
    val typeId = RegisteredTypeId(readVarInt(optimizeForPositive = true))
    ctx.env.typeRegistry.forId(typeId) match {
      case Some(registration) =>
        registration.codec.deserialize()
      case None               =>
        failDeserializerWith(DesertFailure.InvalidTypeId(typeId))
    }
  }

  final def getString(value: StringId)(implicit ctx: DeserializationContext): Option[String] =
    ctx.state.getStringById(value)

  final def storeReadString(value: String)(implicit ctx: DeserializationContext): Unit =
    ctx.state.storeString(value)

  final def getRef(value: RefId)(implicit ctx: DeserializationContext): Option[AnyRef] =
    ctx.state.getRefById(value)

  final def storeReadRef(value: AnyRef)(implicit ctx: DeserializationContext): Unit =
    ctx.state.storeRef(value)

  def readRefOrValue[T <: AnyRef](
      storeReadReference: Boolean = true
  )(implicit codec: BinaryCodec[T], ctx: DeserializationContext): T =
    readVarInt(optimizeForPositive = true) match {
      case 0  =>
        val value = read[T]()
        if (storeReadReference) storeReadRef(value)
        value
      case id =>
        getRef(RefId(id)) match {
          case None        => failDeserializerWith(DesertFailure.InvalidRefId(RefId(id)))
          case Some(value) => value.asInstanceOf[T]
        }
    }
}
