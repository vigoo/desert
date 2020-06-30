package io.github.vigoo.desert

trait BinaryInput {
  def readByte(): Either[DesertFailure, Byte]
  def readShort(): Either[DesertFailure, Short]
  def readInt(): Either[DesertFailure, Int]
  def readLong(): Either[DesertFailure, Long]
  def readBytes(count: Int): Either[DesertFailure, Array[Byte]]

  def readVarInt(optimizeForPositive: Boolean): Either[DesertFailure, Int] = {
    // Based on https://github.com/EsotericSoftware/kryo/blob/master/src/com/esotericsoftware/kryo/io/ByteBufferInput.java#L366

    val readResult = readByte().flatMap { b0 =>
      val r0 = b0 & 0x7F
      if ((b0 & 0x80) != 0) {
        readByte().flatMap { b1 =>
          val r1 = r0 | (b1 & 0x7F) << 7
          if ((b1 & 0x80) != 0) {
            readByte().flatMap { b2 =>
              val r2 = r1 | (b2 & 0x7F) << 14
              if ((b2 & 0x80) != 0) {
                readByte().flatMap { b3 =>
                  val r3 = r2 | (b3 & 0x7F) << 21
                  if ((b3 & 0x80) != 0) {
                    readByte().flatMap { b4 =>
                      val r4 = r3 | (b4 & 0x7F) << 28
                      Right(r4)
                    }
                  } else {
                    Right(r3)
                  }
                }
              } else {
                Right(r2)
              }
            }
          } else {
            Right(r1)
          }
        }
      } else {
        Right(r0)
      }
    }

    readResult.map { r =>
      if (optimizeForPositive) r else (r >>> 1) ^ -(r & 1)
    }
  }
}
