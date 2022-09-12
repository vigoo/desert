package io.github.vigoo.desert.shapeless

import io.github.vigoo.desert.{BinaryCodec, UnknownTypeSerializationSpecBase}
import io.github.vigoo.desert.codecs._

object UnknownTypeSerializationSpec extends UnknownTypeSerializationSpecBase {
  override implicit val firstCodec: BinaryCodec[UnknownTypeSerializationSpec.First]             = DerivedBinaryCodec.derive()
  override implicit val secondCodec: BinaryCodec[UnknownTypeSerializationSpec.Second]           = DerivedBinaryCodec.derive()
  override implicit val testProductCodec: BinaryCodec[UnknownTypeSerializationSpec.TestProduct] =
    DerivedBinaryCodec.derive()
}
