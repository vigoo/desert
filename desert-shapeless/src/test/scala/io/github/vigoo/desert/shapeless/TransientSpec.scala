package io.github.vigoo.desert.shapeless

import io.github.vigoo.desert.{BinaryCodec, FieldAdded, TransientSpecBase}
import io.github.vigoo.desert.TransientSpecBase._
import io.github.vigoo.desert.codecs._

object TransientSpec extends TransientSpecBase {
  override implicit val ttCodec: BinaryCodec[TransientTest]         =
    DerivedBinaryCodec.derive(
      FieldAdded("c", None)
    )
  override implicit val case1Codec: BinaryCodec[Case1]              = DerivedBinaryCodec.derive()
  override implicit val case3Codec: BinaryCodec[Case3]              = DerivedBinaryCodec.derive()
  override implicit val swtCodec: BinaryCodec[SumWithTransientCons] = DerivedBinaryCodec.derive()
}
