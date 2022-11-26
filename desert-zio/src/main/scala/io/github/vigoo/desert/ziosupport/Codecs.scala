package io.github.vigoo.desert.ziosupport

import io.github.vigoo.desert._
import io.github.vigoo.desert.ziosupport.custom._
import io.github.vigoo.desert.BinaryCodec
import io.github.vigoo.desert.custom._
import zio.Chunk

trait Codecs extends LowerPriorityCodecs {

  implicit val byteChunkCodec: BinaryCodec[Chunk[Byte]] = BinaryCodec.define[Chunk[Byte]](chunk =>
    for {
      _ <- writeVarInt(chunk.size, optimizeForPositive = true)
      _ <- writeByteChunk(chunk)
    } yield ()
  )(
    for {
      size  <- readVarInt(optimizeForPositive = true)
      chunk <- readByteChunk(size)
    } yield chunk
  )
}
