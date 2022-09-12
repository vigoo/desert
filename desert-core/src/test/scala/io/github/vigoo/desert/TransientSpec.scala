package io.github.vigoo.desert

import io.github.vigoo.desert.codecs._
import zio.test._

trait TransientSpecBase extends ZIOSpecDefault with SerializationProperties {
  import TransientSpecBase._

  private implicit val typeRegistry: TypeRegistry = TypeRegistry.empty

  implicit val ttCodec: BinaryCodec[TransientTest]

  implicit val case1Codec: BinaryCodec[Case1]
  // Derive must not require implicit codec for the transient Case2
  implicit val case3Codec: BinaryCodec[Case3]
  implicit val swtCodec: BinaryCodec[SumWithTransientCons]

  override def spec: Spec[TestEnvironment, Any] =
    suite("Support for transient modifiers")(
      test("does not serialize a transient field") {
        canBeSerializedAndReadBack(
          TransientTest(1, "2", Some(3), d = true, TypeWithoutCodec(100)),
          TransientTest(1, "def", None, d = true, TypeWithoutCodec(0))
        )
      },
      test("correctly serializes types with transient constructors") {
        canBeSerializedAndReadBack(
          Case1(25),
          Case1(25)
        ) &&
        canBeSerializedAndReadBack(
          Case3("hello"),
          Case3("hello")
        )
      },
      test("serializing a transient constructor fails") {
        cannotBeSerializedAndReadBack[SumWithTransientCons, SumWithTransientCons](Case2(TypeWithoutCodec(1)))
      }
    )
}

object TransientSpecBase {
  case class TypeWithoutCodec(value: Int)

  case class TransientTest(
      a: Int,
      @TransientField("def") b: String,
      @TransientField(None) c: Option[Int],
      d: Boolean,
      @TransientField(TypeWithoutCodec(0)) e: TypeWithoutCodec
  ) // derive must not require an implicit codec for transient fields!

  sealed trait SumWithTransientCons

  case class Case1(a: Int)                                       extends SumWithTransientCons
  @TransientConstructor case class Case2(data: TypeWithoutCodec) extends SumWithTransientCons
  case class Case3(x: String)                                    extends SumWithTransientCons
}
