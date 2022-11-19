package io.github.vigoo.desert.zioschema

import io.github.vigoo.desert.BinaryCodec
import zio.schema.{ Derive, Deriver, Schema }

trait DerivedBinaryCodecVersionSpecific {
  lazy val deriver: Deriver[BinaryCodec]

  inline def derive[T](implicit schema: Schema[T]): BinaryCodec[T] = Derive.derive[BinaryCodec, T](DerivedBinaryCodec.deriver)
}
