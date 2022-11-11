package io.github.vigoo.desert.shapeless

import io.github.vigoo.desert._
import io.github.vigoo.desert.codecs._

object CoproductSpec extends CoproductSpecBase {
  override implicit val v1c1codec: BinaryCodec[Cons1V1.type] = DerivedBinaryCodec.derive()
  override implicit val v1c2codec: BinaryCodec[Cons2V1]      = DerivedBinaryCodec.derive()
  override implicit val v1codec: BinaryCodec[TypeV1]         = DerivedBinaryCodec.derive()

  override implicit val v2c1codec: BinaryCodec[Cons1V2] = DerivedBinaryCodec.derive()
  override implicit val v2c2codec: BinaryCodec[Cons2V2] = DerivedBinaryCodec.derive()
  override implicit val v2c3codec: BinaryCodec[Cons3V2] = DerivedBinaryCodec.derive()
  override implicit val v2codec: BinaryCodec[TypeV2]    = DerivedBinaryCodec.derive()
}
