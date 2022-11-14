package io.github.vigoo.desert

import io.github.vigoo.desert.codecs._
import zio.test._

trait TransientSpecBase extends ZIOSpecDefault with SerializationProperties {
  import TransientSpecBase._

  private implicit val typeRegistry: TypeRegistry = TypeRegistry.empty

  implicit val ttCodec: BinaryCodec[TransientTest]
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
          Case1(25): SumWithTransientCons,
          Case1(25): SumWithTransientCons
        ) &&
        canBeSerializedAndReadBack(
          Case3("hello"): SumWithTransientCons,
          Case3("hello"): SumWithTransientCons
        )
      },
      test("serializing a transient constructor fails") {
        cannotBeSerializedAndReadBack[SumWithTransientCons, SumWithTransientCons](Case2(TypeWithoutCodec(1)))
      }
    )
}

object TransientSpecBase {
  case class TypeWithoutCodec(value: Int)

  @evolutionSteps(FieldAdded("c", None))
  case class TransientTest(
      a: Int,
      @transientField("def") b: String,
      @transientField(None) c: Option[Int],
      d: Boolean,
      @transientField(TypeWithoutCodec(0)) e: TypeWithoutCodec
  ) // derive must not require an implicit codec for transient fields!

  sealed trait SumWithTransientCons

  case class Case1(a: Int)                                       extends SumWithTransientCons
  @transientConstructor case class Case2(data: TypeWithoutCodec) extends SumWithTransientCons
  case class Case3(x: String)                                    extends SumWithTransientCons
}
