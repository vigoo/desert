package io.github.vigoo.desert

import io.github.vigoo.desert.CoproductSpecBase.{Cons1V1, Cons1V2, Cons2V1, Cons2V2, Cons2V3, TypeV1, TypeV2}
import io.github.vigoo.desert.codecs._
import zio.test._
import zio.test.magnolia.DeriveGen

trait CoproductSpecBase extends ZIOSpecDefault with SerializationProperties {

  implicit val v1codec: BinaryCodec[TypeV1]
  val v1gen: Gen[Sized, TypeV1] = DeriveGen[TypeV1]

  implicit val v2codec: BinaryCodec[TypeV2]
  val v2gen: Gen[Sized, TypeV2] = DeriveGen[TypeV2]

  private implicit val typeRegistry: TypeRegistry = TypeRegistry.empty

  override def spec: Spec[TestEnvironment, Any] =
    suite("Coproduct codec derivation")(
      test("serialization works")(canBeSerialized(v1gen)),
      test("can read old data after adding new constructor")(
        canBeSerializedAndReadBack[TypeV1, TypeV2](Cons1V1, Cons1V2())
      ),
      test("can read new data after adding new constructor if it is not the new one")(
        canBeSerializedAndReadBack[TypeV2, TypeV1](Cons2V2("x"), Cons2V1("x"))
      ),
      test("can read new data after adding new constructor if it is not the new one")(
        cannotBeSerializedAndReadBack[TypeV2, TypeV1](Cons2V3(1))
      )
    )
}

object CoproductSpecBase {
  sealed trait TypeV1

  case object Cons1V1 extends TypeV1

  final case class Cons2V1(value: String) extends TypeV1

  sealed trait TypeV2

  final case class Cons1V2() extends TypeV2

  final case class Cons2V2(value: String) extends TypeV2

  final case class Cons2V3(value: Int) extends TypeV2

}
