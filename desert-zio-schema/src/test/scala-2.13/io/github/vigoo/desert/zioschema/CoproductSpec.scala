package io.github.vigoo.desert.zioschema

import io.github.vigoo.desert.CoproductSpecBase.{TypeV1, TypeV2}
import io.github.vigoo.desert.{BinaryCodec, CoproductSpecBase}
import zio.schema.{DeriveSchema, Schema}

object CoproductSpec extends CoproductSpecBase {
  implicit val v1Schema: Schema[TypeV1] = DeriveSchema.gen
  implicit val v2Schema: Schema[TypeV2] = DeriveSchema.gen

  override implicit val v1codec: BinaryCodec[TypeV1] = DerivedBinaryCodec.derive
  override implicit val v2codec: BinaryCodec[TypeV2] = DerivedBinaryCodec.derive
}
