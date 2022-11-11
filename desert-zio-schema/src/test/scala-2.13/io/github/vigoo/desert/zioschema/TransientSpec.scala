package io.github.vigoo.desert.zioschema

import io.github.vigoo.desert.TransientSpecBase.{SumWithTransientCons, TransientTest}
import io.github.vigoo.desert.{BinaryCodec, TransientSpecBase}
import zio.schema.{DeriveSchema, Schema}

object TransientSpec extends TransientSpecBase {
  implicit val ttSchema: Schema[TransientTest]         = DeriveSchema.gen[TransientTest]
  implicit val swtSchema: Schema[SumWithTransientCons] = DeriveSchema.gen[SumWithTransientCons]

  override implicit val ttCodec: BinaryCodec[TransientTest]         = DerivedBinaryCodec.derive[TransientTest]
  override implicit val swtCodec: BinaryCodec[SumWithTransientCons] = DerivedBinaryCodec.derive[SumWithTransientCons]
}
