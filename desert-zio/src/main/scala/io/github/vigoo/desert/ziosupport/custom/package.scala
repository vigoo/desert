package io.github.vigoo.desert.ziosupport

import io.github.vigoo.desert._
import io.github.vigoo.desert.custom.{DeserializationContext, SerializationContext, readBytes, writeBytes}
import zio.Chunk

package object custom {
  final def writeByteChunk[A](chunk: Chunk[Byte])(implicit ctx: SerializationContext): Unit =
    writeBytes(chunk.toArray)

  final def readByteChunk[A](count: Int)(implicit ctx: DeserializationContext): Chunk[Byte] =
    Chunk.fromArray(readBytes(count))
}
