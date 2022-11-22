package io.github.vigoo.desert.zioschema

import io.github.vigoo.desert.{BinaryCodec, DefaultTypeRegistry, SerializationProperties, TypeRegistry}
import io.github.vigoo.desert.zioschema.schemas._
import zio.schema.{DeriveSchema, Schema}
import zio.test.{Spec, TestEnvironment, ZIOSpecDefault}

object UnknownTypeSerializationSpec extends ZIOSpecDefault with SerializationProperties {

  trait CommonInterface

  object CommonInterface {
    implicit val ciCodec: BinaryCodec[CommonInterface] = BinaryCodec.unknown
  }

  case class First(id: String)  extends CommonInterface
  case class Second(value: Int) extends CommonInterface
  case class TestProduct(something: CommonInterface)

  implicit val commonSchema: Schema[CommonInterface] = codecFromTypeRegistry

  implicit val schemaFirst: Schema[First]             = DeriveSchema.gen[First]
  implicit val schemaSecond: Schema[Second]           = DeriveSchema.gen[Second]
  implicit val schemaTestProduct: Schema[TestProduct] = DeriveSchema.gen[TestProduct]

  implicit val firstCodec: BinaryCodec[First]             = DerivedBinaryCodec.derive[First]
  implicit val secondCodec: BinaryCodec[Second]           = DerivedBinaryCodec.derive[Second]
  implicit val testProductCodec: BinaryCodec[TestProduct] = DerivedBinaryCodec.derive[TestProduct]

  override def spec: Spec[TestEnvironment, Any] =
    suite("Serializing types not known at compile time")(
      test("works on top level with type registry") {
        implicit val registry: TypeRegistry = DefaultTypeRegistry()
          .register[First]
          .register[Second]
          .freeze()

        canBeSerializedAndReadBackWithTypeTag(First("hello"), First("hello")) &&
        canBeSerializedAndReadBackWithTypeTag(Second(5), Second(5))
      },
      test("works for derived codecs if interface has proper codec") {
        implicit val registry: TypeRegistry = DefaultTypeRegistry()
          .register[First]
          .register[Second]
          .freeze()

        canBeSerializedAndReadBack(TestProduct(First("hello")), TestProduct(First("hello"))) &&
        canBeSerializedAndReadBack(TestProduct(Second(1)), TestProduct(Second(1)))
      }
    )

}
