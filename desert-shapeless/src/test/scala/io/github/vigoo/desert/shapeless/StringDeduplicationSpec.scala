package io.github.vigoo.desert.shapeless

import io.github.vigoo.desert._
import io.github.vigoo.desert.codecs._

object StringDeduplicationSpec extends StringDeduplicationSpecBase {
  override implicit val v1codec: BinaryCodec[StringDeduplicationSpec.DataV1]       = DerivedBinaryCodec.derive()
  override implicit val v2codec: BinaryCodec[StringDeduplicationSpec.DataV2]       =
    DerivedBinaryCodec.derive(FieldAdded[String]("newField", "unknown"))
  override implicit val outerv1codec: BinaryCodec[StringDeduplicationSpec.OuterV1] = DerivedBinaryCodec.derive()
  override implicit val outerv2codec: BinaryCodec[StringDeduplicationSpec.OuterV2] = DerivedBinaryCodec.derive()
}
