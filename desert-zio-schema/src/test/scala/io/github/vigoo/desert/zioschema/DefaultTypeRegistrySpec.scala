package io.github.vigoo.desert.zioschema

import io.github.vigoo.desert.TypeRegistry.RegisteredTypeId
import io.github.vigoo.desert._
import zio.schema.{DeriveSchema, Schema}
import zio.test.Assertion.{equalTo, isNone, isSome}
import zio.test.{Spec, TestEnvironment, ZIOSpecDefault, assert}

object DefaultTypeRegistrySpec extends ZIOSpecDefault {
  case class TestProd(x: Double, y: Double)

  trait TestInterface

  class Impl1 extends TestInterface

  class Impl2 extends TestInterface

  object TestInterface {

    import BinaryDeserializerOps._
    import BinarySerializerOps._

    implicit val codec: BinaryCodec[TestInterface] =
      BinaryCodec.define[TestInterface] {
        case _: Impl1 => writeByte(1)
        case _: Impl2 => writeByte(2)
      }(readByte().flatMap {
        case 1 => finishDeserializerWith(new Impl1)
        case 2 => finishDeserializerWith(new Impl2)
        case _ =>
          failDeserializerWith(DeserializationFailure("Invalid type tag in custom TestInterface serializer", None))
      })
  }

  implicit val schema: Schema[TestProd]     = DeriveSchema.gen
  implicit val codec: BinaryCodec[TestProd] = DerivedBinaryCodec.derive

  override def spec: Spec[TestEnvironment, Any] =
    suite("DefaultTypeRegistry")(
      test("can register and retrieve codecs for types") {
        implicit val stringCodec: BinaryCodec[String]   = codecs.stringCodec
        implicit val doubleCodec: BinaryCodec[Double]   = codecs.doubleCodec
        implicit val setCodec: BinaryCodec[Set[Double]] = codecs.setCodec[Double]

        val registry = DefaultTypeRegistry()
          .register[String]
          .register[TestProd]
          .register[TestInterface]
          .register[Set[Double]]
          .freeze()

        val regString  = registry.get("hello")
        val regProd    = registry.get(TestProd(0.0, 0.0))
        val regIface   = registry.get(new Impl2)
        val regSet     = registry.get(Set(0.1, 0.2))
        val regUnknown = registry.get(Vector.empty)

        assert(regString)(isSome(equalTo(RegisteredType(RegisteredTypeId(1), stringCodec, classOf[String])))) &&
        assert(regProd)(isSome(equalTo(RegisteredType(RegisteredTypeId(2), codec, classOf[TestProd])))) &&
        assert(regIface)(
          isSome(equalTo(RegisteredType(RegisteredTypeId(3), TestInterface.codec, classOf[TestInterface])))
        ) &&
        assert(regSet)(isSome(equalTo(RegisteredType(RegisteredTypeId(4), setCodec, classOf[Set[Double]])))) &&
        assert(regUnknown)(isNone) &&
        assert(registry.forId(RegisteredTypeId(1)))(
          isSome(equalTo(RegisteredType(RegisteredTypeId(1), stringCodec, classOf[String])))
        ) &&
        assert(registry.forId(RegisteredTypeId(1000)))(isNone)
      },
      test("can skip deprecated type ids during registration") {
        implicit val stringCodec: BinaryCodec[String]   = codecs.stringCodec
        implicit val doubleCodec: BinaryCodec[Double]   = codecs.doubleCodec
        implicit val setCodec: BinaryCodec[Set[Double]] = codecs.setCodec[Double]

        val registry = DefaultTypeRegistry()
          .register[String]
          .registerPlaceholder()
          .registerPlaceholder()
          .register[Set[Double]]
          .freeze()

        val regString = registry.get("hello")
        val regSet    = registry.get(Set(0.1, 0.2))

        assert(regString)(isSome(equalTo(RegisteredType(RegisteredTypeId(1), stringCodec, classOf[String])))) &&
        assert(regSet)(isSome(equalTo(RegisteredType(RegisteredTypeId(4), setCodec, classOf[Set[Double]]))))
      }
    )
}
