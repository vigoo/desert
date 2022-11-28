package io.github.vigoo.desert

import java.util.zip.Inflater

import scala.util.control.NonFatal

/** Interface for reading binary data
  *
  * The low level read operations for primitive types must be implemented.
  *
  * Also contains some higher level functions such as variable integer decoding and compression support, which have a
  * default implementation based on the primitives.
  */
trait BinaryInput {

  /** Reads one byte from the input
    */
  def readByte(): Byte

  /** Reads a 16-bit integer from the input
    */
  def readShort(): Short

  /** Reads a 32-bit integer from the input
    */
  def readInt(): Int

  /** Reads a 64-bit integer from the input
    */
  def readLong(): Long

  /** Reads a 32-bit floating point value from the input
    */
  def readFloat(): Float

  /** Reads a 64-bit floating point value from the input
    */
  def readDouble(): Double

  /** Reads N bytes from the input into an array
    * @param count
    *   Number of bytes to read
    */
  def readBytes(count: Int): Array[Byte]

  /** Reads a variable-length encoded 32-bit integer from the input
    *
    * The encoding takes 1-5 bytes.
    *
    * Based on
    * https://github.com/EsotericSoftware/kryo/blob/master/src/com/esotericsoftware/kryo/io/ByteBufferInput.java#L366
    *
    * @param optimizeForPositive
    *   If true, the encoding was optimized for positive integers. This parameter must match the one passed to the
    *   [[BinaryOutput.writeVarInt]]
    */
  def readVarInt(optimizeForPositive: Boolean): Int = {
    val r = {
      val b0 = readByte()
      val r0 = b0 & 0x7f
      if ((b0 & 0x80) != 0) {
        val b1 = readByte()
        val r1 = r0 | (b1 & 0x7f) << 7
        if ((b1 & 0x80) != 0) {
          val b2 = readByte()
          val r2 = r1 | (b2 & 0x7f) << 14
          if ((b2 & 0x80) != 0) {
            val b3 = readByte()
            val r3 = r2 | (b3 & 0x7f) << 21
            if ((b3 & 0x80) != 0) {
              val b4 = readByte()
              r3 | (b4 & 0x7f) << 28
            } else {
              r3
            }
          } else {
            r2
          }
        } else {
          r1
        }
      } else {
        r0
      }
    }

    if (optimizeForPositive) r else (r >>> 1) ^ -(r & 1)
  }

  /** Reads a compressed byte array from the input
    *
    * It assumes to have two variable-length integer representing the uncompressed and the compressed data length
    * followed by the ZIP-compressed array of bytes. Counterpart of [[BinaryOutput.writeCompressedByteArray]]
    */
  def readCompressedByteArray(): Array[Byte] = {
    val uncompressedLength = readVarInt(optimizeForPositive = true)
    if (uncompressedLength == 0) {
      Array[Byte]()
    } else {
      val compressedLength = readVarInt(optimizeForPositive = true)
      val compressedData   = readBytes(compressedLength)
      try {
        val inflater = new Inflater()
        try {
          inflater.setInput(compressedData)
          val uncompressedData = new Array[Byte](uncompressedLength)
          inflater.inflate(uncompressedData)
          uncompressedData
        } finally
          inflater.end()
      } catch {
        case NonFatal(failure) =>
          throw DesertException(
            DesertFailure.DeserializationFailure("Failed to uncompress byte array", Some(failure))
          )
      }
    }
  }
}
