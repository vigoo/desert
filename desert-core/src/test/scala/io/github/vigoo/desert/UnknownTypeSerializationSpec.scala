package io.github.vigoo.desert

import io.github.vigoo.desert.codecs._
import zio.test._

trait UnknownTypeSerializationSpecBase extends ZIOSpecDefault with SerializationProperties {

  trait CommonInterface
  implicit val ciCodec: BinaryCodec[CommonInterface] = BinaryCodec.unknown

  case class First(id: String) extends CommonInterface
  implicit val firstCodec: BinaryCodec[First]

  case class Second(value: Int) extends CommonInterface
  implicit val secondCodec: BinaryCodec[Second]

  case class TestProduct(something: CommonInterface)
  implicit val testProductCodec: BinaryCodec[TestProduct]

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
