package io.github.vigoo.desert.shapeless

import io.github.vigoo.desert.EvolutionSpecBase.TestId
import io.github.vigoo.desert._
import io.github.vigoo.desert.codecs._

object EvolutionSpec extends EvolutionSpecBase {
  implicit val v1codec: BinaryCodec[ProdV1]               = DerivedBinaryCodec.derive()
  override implicit val v2codec: BinaryCodec[ProdV2]      = DerivedBinaryCodec.derive(ProdV2.steps: _*)
  override implicit val v3codec: BinaryCodec[ProdV3]      = DerivedBinaryCodec.derive(ProdV3.steps: _*)
  override implicit val v4codec: BinaryCodec[ProdV4]      = DerivedBinaryCodec.derive(ProdV4.steps: _*)
  override implicit val v5codec: BinaryCodec[ProdV5]      = DerivedBinaryCodec.derive(ProdV5.steps: _*)
  override implicit val c1case1Codec: BinaryCodec[Case11] = DerivedBinaryCodec.derive()
  override implicit val c1case2Codec: BinaryCodec[Case21] = DerivedBinaryCodec.derive()
  override implicit val c1codec: BinaryCodec[Coprod1]     = DerivedBinaryCodec.derive()
  override implicit val c2case1Codec: BinaryCodec[Case12] = DerivedBinaryCodec.derive()
  override implicit val c2case2Codec: BinaryCodec[Case22] = DerivedBinaryCodec.derive()
  override implicit val c2codec: BinaryCodec[Coprod2]     = DerivedBinaryCodec.derive()
  override implicit val testIdCodec: BinaryCodec[TestId]  = DerivedBinaryCodec.deriveForWrapper
}
