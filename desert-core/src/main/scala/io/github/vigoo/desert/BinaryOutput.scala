package io.github.vigoo.desert

import zio.ZIO

trait BinaryOutput {
  def writeByte(value: Byte): ZIO[Any, DesertFailure, Unit]
  def writeShort(value: Short): ZIO[Any, DesertFailure, Unit]
  def writeInt(value: Int): ZIO[Any, DesertFailure, Unit]
  def writeLong(value: Long): ZIO[Any, DesertFailure, Unit]
  def writeFloat(value: Float): ZIO[Any, DesertFailure, Unit]
  def writeDouble(value: Double): ZIO[Any, DesertFailure, Unit]
  def writeBytes(value: Array[Byte]): ZIO[Any, DesertFailure, Unit]

  def writeVarInt(value: Int, optimizeForPositive: Boolean = false): ZIO[Any, DesertFailure, Unit] = {
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
}
