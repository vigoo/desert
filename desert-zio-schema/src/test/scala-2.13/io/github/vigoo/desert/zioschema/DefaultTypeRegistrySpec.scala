package io.github.vigoo.desert.zioschema

import io.github.vigoo.desert.DefaultTypeRegistrySpecBase.TestProd
import io.github.vigoo.desert.{BinaryCodec, DefaultTypeRegistrySpecBase}
import zio.schema.{DeriveSchema, Schema}

object DefaultTypeRegistrySpec extends DefaultTypeRegistrySpecBase {
  implicit val schema: Schema[TestProd]              = DeriveSchema.gen
  override implicit val codec: BinaryCodec[TestProd] = DerivedBinaryCodec.derive
}
