package io.github.vigoo.desert

import zio._

trait BinaryInput {
  def readByte(): ZIO[Any, DesertFailure, Byte]
  def readShort(): ZIO[Any, DesertFailure, Short]
  def readInt(): ZIO[Any, DesertFailure, Int]
  def readLong(): ZIO[Any, DesertFailure, Long]
  def readFloat(): ZIO[Any, DesertFailure, Float]
  def readDouble(): ZIO[Any, DesertFailure, Double]
  def readBytes(count: Int): ZIO[Any, DesertFailure, Array[Byte]]

  def readVarInt(optimizeForPositive: Boolean): ZIO[Any, DesertFailure, Int] = {
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
                      ZIO.succeed(r4)
                    }
                  } else {
                    ZIO.succeed(r3)
                  }
                }
              } else {
                ZIO.succeed(r2)
              }
            }
          } else {
            ZIO.succeed(r1)
          }
        }
      } else {
        ZIO.succeed(r0)
      }
    }

    readResult.map { r =>
      if (optimizeForPositive) r else (r >>> 1) ^ -(r & 1)
    }
  }
}
