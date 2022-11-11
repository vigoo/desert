package io.github.vigoo.desert

import io.github.vigoo.desert.codecs._
import zio.test._
import zio.test.magnolia.DeriveGen

trait CoproductSpecBase extends ZIOSpecDefault with SerializationProperties {
  sealed trait TypeV1
  final case object Cons1V1               extends TypeV1
  final case class Cons2V1(value: String) extends TypeV1

  implicit val v1c1codec: BinaryCodec[Cons1V1.type]
  implicit val v1c2codec: BinaryCodec[Cons2V1]
  implicit val v1codec: BinaryCodec[TypeV1]
  val v1gen: Gen[Sized, TypeV1] = DeriveGen[TypeV1]

  sealed trait TypeV2
  final case class Cons1V2()              extends TypeV2
  final case class Cons2V2(value: String) extends TypeV2
  final case class Cons3V2(value: Int)    extends TypeV2

  implicit val v2c1codec: BinaryCodec[Cons1V2]
  implicit val v2c2codec: BinaryCodec[Cons2V2]
  implicit val v2c3codec: BinaryCodec[Cons3V2]
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
        cannotBeSerializedAndReadBack[TypeV2, TypeV1](Cons3V2(1))
      )
    )
}
