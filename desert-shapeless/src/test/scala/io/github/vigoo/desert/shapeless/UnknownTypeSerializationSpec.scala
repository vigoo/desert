package io.github.vigoo.desert.shapeless

import io.github.vigoo.desert.{BinaryCodec, DefaultTypeRegistry, SerializationProperties, TypeRegistry}
import zio.test.{Spec, TestEnvironment, ZIOSpecDefault}

object UnknownTypeSerializationSpec extends ZIOSpecDefault with SerializationProperties {

  trait CommonInterface

  object CommonInterface {
    implicit val ciCodec: BinaryCodec[CommonInterface] = BinaryCodec.unknown
  }

  case class First(id: String)  extends CommonInterface
  case class Second(value: Int) extends CommonInterface
  case class TestProduct(something: CommonInterface)

  implicit val firstCodec: BinaryCodec[First]             = DerivedBinaryCodec.derive
  implicit val secondCodec: BinaryCodec[Second]           = DerivedBinaryCodec.derive
  implicit val testProductCodec: BinaryCodec[TestProduct] =
    DerivedBinaryCodec.derive

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
