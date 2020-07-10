package io.github.vigoo.desert

/**
 * Interface for writing binary data
 *
 * The low level write operations for primitive types must be implemented.
 *
 * Also contains some higher level functions such as variable integer encoding and compression support,
 * which have a default implementation based on the primitives.
 */
trait BinaryOutput {
  /**
   * Writes one byte
   * @param value The value to write
   */
  def writeByte(value: Byte): Either[DesertFailure, Unit]

  /**
   * Writes one 16-bit integer
   * @param value The value to write
   */
  def writeShort(value: Short): Either[DesertFailure, Unit]

  /**
   * Writes one 32-bit integer
   * @param value The value to write
   */
  def writeInt(value: Int): Either[DesertFailure, Unit]

  /**
   * Writes one 64-bit integer
   * @param value The value to write
   */
  def writeLong(value: Long): Either[DesertFailure, Unit]

  /**
   * Writes one 32-bit floating point value
   * @param value The value to write
   */
  def writeFloat(value: Float): Either[DesertFailure, Unit]

  /**
   * Writes one 64-bit floating point value
   * @param value The value to write
   */
  def writeDouble(value: Double): Either[DesertFailure, Unit]

  /**
   * Writes an array of bytes (without writing any information about the number of bytes)
   * @param value The bytes to write
   */
  def writeBytes(value: Array[Byte]): Either[DesertFailure, Unit]

  /**
   * Writes a slice of an array of bytes (without writing any infomation about the number of bytes)
   * @param value The byte array
   * @param start Index of the first byte to write
   * @param count Number of bytes to write
   */
  def writeBytes(value: Array[Byte], start: Int, count: Int): Either[DesertFailure, Unit]

  /**
   * Writes a 32-bit integer with a variable-length encoding
   *
   * The number of encoded bytes is 1-5.
   * Based on https://github.com/EsotericSoftware/kryo/blob/master/src/com/esotericsoftware/kryo/io/ByteBufferOutput.java#L290
   *
   * @param value The value to write
   * @param optimizeForPositive If true the encoding is optimized for positive numbers
   */
  def writeVarInt(value: Int, optimizeForPositive: Boolean = false): Either[DesertFailure, Unit] = {
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
