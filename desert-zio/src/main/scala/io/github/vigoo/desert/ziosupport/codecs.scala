package io.github.vigoo.desert.ziosupport

import io.github.vigoo.desert.codecs._
import io.github.vigoo.desert.syntax._
import io.github.vigoo.desert.ziosupport.syntax._
import io.github.vigoo.desert.BinaryCodec
import zio.Chunk

object codecs extends LowerPriorityCodecs {

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
