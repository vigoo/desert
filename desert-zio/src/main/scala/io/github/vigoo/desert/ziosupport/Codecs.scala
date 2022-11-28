package io.github.vigoo.desert.ziosupport

import io.github.vigoo.desert._
import io.github.vigoo.desert.ziosupport.custom._
import io.github.vigoo.desert.BinaryCodec
import io.github.vigoo.desert.custom._
import zio.Chunk

trait Codecs extends LowerPriorityCodecs {

  implicit val byteChunkCodec: BinaryCodec[Chunk[Byte]] = new BinaryCodec[Chunk[Byte]] {
    override def deserialize()(implicit ctx: DeserializationContext): Chunk[Byte] = {
      val size = readVarInt(optimizeForPositive = true)
      readByteChunk(size)
    }

    override def serialize(value: Chunk[Byte])(implicit context: SerializationContext): Unit = {
      writeVarInt(value.size, optimizeForPositive = true)
      writeByteChunk(value)
    }
  }
}
