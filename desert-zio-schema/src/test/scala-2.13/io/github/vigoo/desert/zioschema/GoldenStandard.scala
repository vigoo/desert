package io.github.vigoo.desert.zioschema

import io.github.vigoo.desert.{BinaryCodec, GoldenStandardBase, PersistedThrowable}
import io.github.vigoo.desert.golden.TestModel1
import io.github.vigoo.desert.zioschema.schemas._
import _root_.zio.schema.{DeriveSchema, Schema}

import scala.reflect.ClassTag
import scala.util.Try

object GoldenStandard extends GoldenStandardBase {
  private implicit val listElement1Schema: Schema[TestModel1.ListElement1] = DeriveSchema.gen
  private implicit val listElement2Schema: Schema[TestModel1.ListElement2] = DeriveSchema.gen
  private implicit val testModel1Schema: Schema[TestModel1]                = DeriveSchema.gen
  implicit val testModel1Codec: BinaryCodec[TestModel1]                    = DerivedBinaryCodec.derive
}
