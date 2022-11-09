package io.github.vigoo.desert.shapeless

import io.github.vigoo.desert.UnknownTypeSerializationSpecBase.{First, Second, TestProduct}
import io.github.vigoo.desert.{BinaryCodec, UnknownTypeSerializationSpecBase}
import io.github.vigoo.desert.codecs._

object UnknownTypeSerializationSpec extends UnknownTypeSerializationSpecBase {
  override implicit val firstCodec: BinaryCodec[First]             = DerivedBinaryCodec.derive
  override implicit val secondCodec: BinaryCodec[Second]           = DerivedBinaryCodec.derive
  override implicit val testProductCodec: BinaryCodec[TestProduct] =
    DerivedBinaryCodec.derive
}
