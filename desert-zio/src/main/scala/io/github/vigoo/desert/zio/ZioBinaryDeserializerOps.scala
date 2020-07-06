package io.github.vigoo.desert.zio

import io.github.vigoo.desert.syntax._
import io.github.vigoo.desert.BinaryDeserializer.Deser
import zio.Chunk

trait ZioBinaryDeserializerOps {
  final def readByteChunk[A](count: Int): Deser[Chunk[Byte]] = getInput.flatMap {
    input => input.readBytes(count).map(Chunk.fromArray[Byte])
  }
}

object ZioBinaryDeserializerOps extends ZioBinaryDeserializerOps
