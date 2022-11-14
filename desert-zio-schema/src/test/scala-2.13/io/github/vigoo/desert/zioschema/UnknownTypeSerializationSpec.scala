package io.github.vigoo.desert.zioschema

import io.github.vigoo.desert.UnknownTypeSerializationSpecBase.{CommonInterface, First, Second, TestProduct}
import io.github.vigoo.desert.{BinaryCodec, UnknownTypeSerializationSpecBase}
import io.github.vigoo.desert.zioschema.schemas._
import zio.schema.{DeriveSchema, Schema}

object UnknownTypeSerializationSpec extends UnknownTypeSerializationSpecBase {
  implicit val commonSchema: Schema[CommonInterface] = codecFromTypeRegistry

  implicit val schemaFirst: Schema[First]             = DeriveSchema.gen[First]
  implicit val schemaSecond: Schema[Second]           = DeriveSchema.gen[Second]
  implicit val schemaTestProduct: Schema[TestProduct] = DeriveSchema.gen[TestProduct]

  override implicit val firstCodec: BinaryCodec[First]             = DerivedBinaryCodec.derive[First]
  override implicit val secondCodec: BinaryCodec[Second]           = DerivedBinaryCodec.derive[Second]
  override implicit val testProductCodec: BinaryCodec[TestProduct] = DerivedBinaryCodec.derive[TestProduct]
}
