package io.github.vigoo.desert

import codecs._

import org.junit.runner.RunWith
import zio.test._
import zio.test.Assertion._
import zio.test.environment.TestEnvironment

import scala.annotation.nowarn

@RunWith(classOf[zio.test.junit.ZTestJUnitRunner])
class UnknownTypeSerializationSpec extends DefaultRunnableSpec with SerializationProperties {

  trait CommonInterface
  object CommonInterface {
    implicit val codec: BinaryCodec[CommonInterface] = BinaryCodec.unknown
  }

  case class First(id: String) extends CommonInterface
  object First {
    implicit val codec: BinaryCodec[First] = BinaryCodec.derive()
  }

  case class Second(value: Int) extends CommonInterface
  object Second {
    implicit val codec: BinaryCodec[Second] = BinaryCodec.derive()
  }

  case class TestProduct(something: CommonInterface)
  object TestProduct {
    implicit val codec: BinaryCodec[TestProduct] = BinaryCodec.derive()
  }

  override def spec: ZSpec[TestEnvironment, Any] =
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

@nowarn object UnknownTypeSerializationSpec extends UnknownTypeSerializationSpec
