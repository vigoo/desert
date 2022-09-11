package io.github.vigoo.desert.zio

import io.github.vigoo.desert.BinaryCodec
import io.github.vigoo.desert.codecs._
import zio.Chunk

trait LowerPriorityCodecs {
  implicit def chunkCodec[A: BinaryCodec]: BinaryCodec[Chunk[A]] =
    iterableCodec[A, Chunk[A]](BinaryCodec[A], Chunk.iterableFactory)
}
