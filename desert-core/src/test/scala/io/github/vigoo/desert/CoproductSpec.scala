package io.github.vigoo.desert

import io.github.vigoo.desert.codecs._

import org.junit.runner.RunWith
import zio.random.Random
import zio.test._
import zio.test.magnolia.DeriveGen
import zio.test.environment.TestEnvironment

import scala.annotation.nowarn

@RunWith(classOf[zio.test.junit.ZTestJUnitRunner])
class CoproductSpec extends DefaultRunnableSpec with SerializationProperties {
  sealed trait TypeV1
  final case object Cons1V1 extends TypeV1
  final case class Cons2V1(value: String) extends TypeV1

  object TypeV1 {
    implicit val c1codec: BinaryCodec[Cons1V1.type] = BinaryCodec.derive()
    implicit val c2codec: BinaryCodec[Cons2V1] = BinaryCodec.derive()
    implicit val codec: BinaryCodec[TypeV1] = BinaryCodec.derive()
    val gen: Gen[Random with Sized, TypeV1] = DeriveGen[TypeV1]
  }

  sealed trait TypeV2
  final case class Cons1V2() extends TypeV2
  final case class Cons2V2(value: String) extends TypeV2
  final case class Cons3V2(value: Int) extends TypeV2

  object TypeV2 {
    implicit val c1codec: BinaryCodec[Cons1V2] = BinaryCodec.derive()
    implicit val c2codec: BinaryCodec[Cons2V2] = BinaryCodec.derive()
    implicit val c3codec: BinaryCodec[Cons3V2] = BinaryCodec.derive()
    implicit val codec: BinaryCodec[TypeV2] = BinaryCodec.derive()
    val gen: Gen[Random with Sized, TypeV2] = DeriveGen[TypeV2]
  }

  private implicit val typeRegistry = TypeRegistry.empty

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("Coproduct codec derivation")(
      testM("serialization works")(canBeSerialized(TypeV1.gen)),
      test("can read old data after adding new constructor")(
        canBeSerializedAndReadBack[TypeV1, TypeV2](Cons1V1, Cons1V2())
      ),
      test("can read new data after adding new constructor if it is not the new one")(
        canBeSerializedAndReadBack[TypeV2, TypeV1](Cons2V2("x"), Cons2V1("x"))
      ),
      test("can read new data after adding new constructor if it is not the new one")(
        cannotBeSerializedAndReadBack[TypeV2, TypeV1](Cons3V2(1))
      )
    )
}

@nowarn object CoproductSpec extends CoproductSpec