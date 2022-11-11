package io.github.vigoo.desert.zioschema

import _root_.zio.schema.{DeriveSchema, Schema}
import io.github.vigoo.desert.EvolutionSpecBase._
import io.github.vigoo.desert._

object EvolutionSpec extends EvolutionSpecBase {

  implicit val schemaProdV1: Schema[ProdV1]   = DeriveSchema.gen
  implicit val schemaProdV2: Schema[ProdV2]   = DeriveSchema.gen
  implicit val schemaProdV3: Schema[ProdV3]   = DeriveSchema.gen
  implicit val schemaProdV4: Schema[ProdV4]   = DeriveSchema.gen
  implicit val schemaProdV5: Schema[ProdV5]   = DeriveSchema.gen
  implicit val schemaCoprod1: Schema[Coprod1] = DeriveSchema.gen
  implicit val schemaCoprod2: Schema[Coprod2] = DeriveSchema.gen

  override implicit val v1codec: BinaryCodec[ProdV1]     = DerivedBinaryCodec.derive
  override implicit val v2codec: BinaryCodec[ProdV2]     = DerivedBinaryCodec.derive
  override implicit val v3codec: BinaryCodec[ProdV3]     = DerivedBinaryCodec.derive
  override implicit val v4codec: BinaryCodec[ProdV4]     = DerivedBinaryCodec.derive
  override implicit val v5codec: BinaryCodec[ProdV5]     = DerivedBinaryCodec.derive
  override implicit val c1codec: BinaryCodec[Coprod1]    = DerivedBinaryCodec.derive
  override implicit val c2codec: BinaryCodec[Coprod2]    = DerivedBinaryCodec.derive
  override implicit val testIdCodec: BinaryCodec[TestId] = BinaryCodec.from(
    codecs.stringCodec.contramap(_.value),
    codecs.stringCodec.map(TestId.apply)
  ) // TODO: derivation support
}
