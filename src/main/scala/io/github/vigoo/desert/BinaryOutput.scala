package io.github.vigoo.desert

trait BinaryOutput {
  def writeByte(value: Byte): Either[DesertFailure, Unit]
  def writeShort(value: Short): Either[DesertFailure, Unit]
  def writeInt(value: Int): Either[DesertFailure, Unit]
  def writeLong(value: Long): Either[DesertFailure, Unit]
  def writeFloat(value: Float): Either[DesertFailure, Unit]
  def writeDouble(value: Double): Either[DesertFailure, Unit]
  def writeBytes(value: Array[Byte]): Either[DesertFailure, Unit]

  def writeVarInt(value: Int, optimizeForPositive: Boolean = false): Either[DesertFailure, Unit] = {
    // Based on https://github.com/EsotericSoftware/kryo/blob/master/src/com/esotericsoftware/kryo/io/ByteBufferOutput.java#L290

    val adjustedValue = if (optimizeForPositive) value else (value << 1) ^ (value >> 31)
    if (adjustedValue >>> 7 == 0) {
      writeByte(adjustedValue.toByte)
    } else if (adjustedValue >>> 14 == 0) {
      for {
        _ <- writeByte(((adjustedValue & 0x7F) | 0x80).toByte)
        _ <- writeByte((adjustedValue >>> 7).toByte)
      } yield ()
    } else if (adjustedValue >>> 21 == 0) {
      for {
        _ <- writeByte(((adjustedValue & 0x7F) | 0x80).toByte)
        _ <- writeByte(((adjustedValue >>> 7) | 0x80).toByte)
        _ <- writeByte((adjustedValue >>> 14).toByte)
      } yield ()
    } else if (adjustedValue >>> 28 == 0) {
      for {
        _ <- writeByte(((adjustedValue & 0x7F) | 0x80).toByte)
        _ <- writeByte(((adjustedValue >>> 7) | 0x80).toByte)
        _ <- writeByte(((adjustedValue >>> 14) | 0x80).toByte)
        _ <- writeByte((adjustedValue >>> 21).toByte)
      } yield ()
    } else {
      for {
        _ <- writeByte(((adjustedValue & 0x7F) | 0x80).toByte)
        _ <- writeByte(((adjustedValue >>> 7) | 0x80).toByte)
        _ <- writeByte(((adjustedValue >>> 14) | 0x80).toByte)
        _ <- writeByte(((adjustedValue >>> 21) | 0x80).toByte)
        _ <- writeByte((adjustedValue >>> 28).toByte)
      } yield ()
    }
  }

  def writeUnknown(value: Any, typeRegistry: TypeRegistry): Either[DesertFailure, Unit] =
    ???
}
