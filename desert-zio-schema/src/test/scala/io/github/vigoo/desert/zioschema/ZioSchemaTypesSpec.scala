package io.github.vigoo.desert.zioschema

import io.github.vigoo.desert.{BinaryCodec, SerializationProperties}
import zio.Scope
import zio.schema.meta.MetaSchema
import zio.schema.{DeriveGen, DynamicValue, Schema}
import zio.test.{Gen, Spec, TestEnvironment, ZIOSpecDefault}

object ZioSchemaTypesSpec extends ZIOSpecDefault with SerializationProperties {
  override def spec: Spec[TestEnvironment with Scope, Any] =
    suite("Serialization of zio-schema specific types")(
//      test("MetaSchema") {
//        implicit val metaSchemaCodec: BinaryCodec[MetaSchema] =
//          DerivedBinaryCodec.derive[MetaSchema]
//
//        canBeSerialized(
//          Gen.oneOf(
//            Gen.const(MetaSchema.schema.ast),
//            Gen.const(Schema.dynamicValue.ast)
//          )
//        )
//      }
//      test("DynamicValue") {
//        implicit val dynamicValueCodec: BinaryCodec[DynamicValue] =
//          DerivedBinaryCodec.derive(Schema.dynamicValue)
//
//        canBeSerialized(DeriveGen.gen[DynamicValue])
//      }
    )
}
