package io.github.vigoo.desert.zioschema

import io.github.vigoo.desert.StringDeduplicationSpecBase._
import io.github.vigoo.desert.{BinaryCodec, StringDeduplicationSpecBase}
import zio.schema.{DeriveSchema, Schema}

object StringDeduplicationSpec extends StringDeduplicationSpecBase {
  implicit val v1Schema: Schema[DataV1]       = DeriveSchema.gen
  implicit val v2Schema: Schema[DataV2]       = DeriveSchema.gen
  implicit val outerv1Schema: Schema[OuterV1] = DeriveSchema.gen
  implicit val outerv2Schema: Schema[OuterV2] = DeriveSchema.gen

  override implicit val v1codec: BinaryCodec[DataV1]       = DerivedBinaryCodec.derive
  override implicit val v2codec: BinaryCodec[DataV2]       = DerivedBinaryCodec.derive
  override implicit val outerv1codec: BinaryCodec[OuterV1] = DerivedBinaryCodec.derive
  override implicit val outerv2codec: BinaryCodec[OuterV2] = DerivedBinaryCodec.derive
}
