package io.github.vigoo.desert

import java.util.zip.Deflater

import scala.collection.mutable.ArrayBuffer
import scala.util.control.NonFatal

/** Interface for writing binary data
  *
  * The low level write operations for primitive types must be implemented.
  *
  * Also contains some higher level functions such as variable integer encoding and compression support, which have a
  * default implementation based on the primitives.
  */
trait BinaryOutput {
  private val buffer: Array[Byte] = new Array[Byte](16)

  /** Writes one byte
    * @param value
    *   The value to write
    */
  def writeByte(value: Byte): Either[DesertFailure, Unit]

  /** Writes one 16-bit integer
    * @param value
    *   The value to write
    */
  def writeShort(value: Short): Either[DesertFailure, Unit]

  /** Writes one 32-bit integer
    * @param value
    *   The value to write
    */
  def writeInt(value: Int): Either[DesertFailure, Unit]

  /** Writes one 64-bit integer
    * @param value
    *   The value to write
    */
  def writeLong(value: Long): Either[DesertFailure, Unit]

  /** Writes one 32-bit floating point value
    * @param value
    *   The value to write
    */
  def writeFloat(value: Float): Either[DesertFailure, Unit]

  /** Writes one 64-bit floating point value
    * @param value
    *   The value to write
    */
  def writeDouble(value: Double): Either[DesertFailure, Unit]

  /** Writes an array of bytes (without writing any information about the number of bytes)
    * @param value
    *   The bytes to write
    */
  def writeBytes(value: Array[Byte]): Either[DesertFailure, Unit]

  /** Writes a slice of an array of bytes (without writing any infomation about the number of bytes)
    * @param value
    *   The byte array
    * @param start
    *   Index of the first byte to write
    * @param count
    *   Number of bytes to write
    */
  def writeBytes(value: Array[Byte], start: Int, count: Int): Either[DesertFailure, Unit]

  /** Writes a 32-bit integer with a variable-length encoding
    *
    * The number of encoded bytes is 1-5. Based on
    * https://github.com/EsotericSoftware/kryo/blob/master/src/com/esotericsoftware/kryo/io/ByteBufferOutput.java#L290
    *
    * @param value
    *   The value to write
    * @param optimizeForPositive
    *   If true the encoding is optimized for positive numbers
    */
  def writeVarInt(value: Int, optimizeForPositive: Boolean = false): Either[DesertFailure, Unit] = {
    val adjustedValue = if (optimizeForPositive) value else (value << 1) ^ (value >> 31)
    if (adjustedValue >>> 7 == 0) {
      writeByte(adjustedValue.toByte)
    } else if (adjustedValue >>> 14 == 0) {
      buffer(0) = ((adjustedValue & 0x7f) | 0x80).toByte
      buffer(1) = (adjustedValue >>> 7).toByte
      writeBytes(buffer, 0, 2)
    } else if (adjustedValue >>> 21 == 0) {
      buffer(0) = ((adjustedValue & 0x7f) | 0x80).toByte
      buffer(1) = ((adjustedValue >>> 7) | 0x80).toByte
      buffer(2) = (adjustedValue >>> 14).toByte
      writeBytes(buffer, 0, 3)
    } else if (adjustedValue >>> 28 == 0) {
      buffer(0) = ((adjustedValue & 0x7f) | 0x80).toByte
      buffer(1) = ((adjustedValue >>> 7) | 0x80).toByte
      buffer(2) = ((adjustedValue >>> 14) | 0x80).toByte
      buffer(3) = (adjustedValue >>> 21).toByte
      writeBytes(buffer, 0, 4)
    } else {
      buffer(0) = ((adjustedValue & 0x7f) | 0x80).toByte
      buffer(1) = ((adjustedValue >>> 7) | 0x80).toByte
      buffer(2) = ((adjustedValue >>> 14) | 0x80).toByte
      buffer(3) = ((adjustedValue >>> 21) | 0x80).toByte
      buffer(4) = (adjustedValue >>> 28).toByte
      writeBytes(buffer, 0, 5)
    }
  }

  /** Compress the given byte array with ZIP and write write the compressed data to the output
    *
    * The compressed data is prepended with the uncompressed and the compressed data sizes, encoded with variable-length
    * integer encoding.
    *
    * Use the [[BinaryInput.readCompressedByteArray]] function to read it back.
    *
    * @param uncompressedData
    *   Uncompressed data
    * @param level
    *   Compression level. Use constants from the [[Deflater]] class
    */
  def writeCompressedByteArray(
      uncompressedData: Array[Byte],
      level: Int = Deflater.BEST_SPEED
  ): Either[DesertFailure, Unit] =
    if (uncompressedData.length == 0) {
      writeVarInt(0, optimizeForPositive = true)
    } else {
      try {
        val deflater = new Deflater(level)
        deflater.setInput(uncompressedData)
        deflater.finish()

        val compressedData   = new ArrayBuffer[Array[Byte]](4)
        val buffer           = new Array[Byte](uncompressedData.length)
        var compressedLength = 0
        while (!deflater.finished()) {
          val length = deflater.deflate(buffer)
          compressedData.addOne(buffer.slice(0, length))
          compressedLength += length
        }

        deflater.end()
        for {
          _ <- writeVarInt(uncompressedData.length, optimizeForPositive = true)
          _ <- writeVarInt(compressedLength, optimizeForPositive = true)
          _ <- compressedData.foldLeft[Either[DesertFailure, Unit]](Right(())) { case (r, bs) =>
                 r.flatMap(_ => writeBytes(bs))
               }
        } yield ()
      } catch {
        case NonFatal(failure) => Left(SerializationFailure("Failed to compress data", Some(failure)))
      }
    }
}
