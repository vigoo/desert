package io.github.vigoo.desert.ziosupport

import io.github.vigoo.desert._
import zio.Chunk

trait LowerPriorityCodecs {
  implicit def chunkCodec[A: BinaryCodec]: BinaryCodec[Chunk[A]] =
    iterableCodec[A, Chunk[A]](BinaryCodec[A], Chunk.iterableFactory)
}
