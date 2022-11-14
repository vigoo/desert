package io.github.vigoo.desert.shapeless

import io.github.vigoo.desert.StringDeduplicationSpecBase._
import io.github.vigoo.desert._
import io.github.vigoo.desert.codecs._

object StringDeduplicationSpec extends StringDeduplicationSpecBase {
  override implicit val v1codec: BinaryCodec[DataV1]       = DerivedBinaryCodec.derive
  override implicit val v2codec: BinaryCodec[DataV2]       = DerivedBinaryCodec.derive
  override implicit val outerv1codec: BinaryCodec[OuterV1] = DerivedBinaryCodec.derive
  override implicit val outerv2codec: BinaryCodec[OuterV2] = DerivedBinaryCodec.derive
}
