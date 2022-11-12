package io.github.vigoo.desert.shapeless

import io.github.vigoo.desert.golden.TestModel1
import io.github.vigoo.desert.{BinaryCodec, FieldAdded, FieldMadeOptional, FieldMadeTransient, GoldenStandardBase}
import io.github.vigoo.desert.codecs._

object GoldenStandard extends GoldenStandardBase {

  private implicit val listElement1Codec: BinaryCodec[TestModel1.ListElement1]              = DerivedBinaryCodec.derive()
  private implicit val listElement2FirstCodec: BinaryCodec[TestModel1.ListElement2.First]   = DerivedBinaryCodec.derive()
  private implicit val listElement2SecondCodec: BinaryCodec[TestModel1.ListElement2.Second] =
    DerivedBinaryCodec.derive(FieldMadeTransient("cached"))
  private implicit val listElement2Codec: BinaryCodec[TestModel1.ListElement2]              = DerivedBinaryCodec.derive()

  implicit val testModel1Codec: BinaryCodec[TestModel1] = DerivedBinaryCodec.derive(
    FieldMadeOptional("option"),
    FieldAdded("string", "default string"),
    FieldAdded("set", Set.empty[String])
  )
}
