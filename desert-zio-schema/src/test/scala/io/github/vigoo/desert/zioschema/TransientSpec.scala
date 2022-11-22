package io.github.vigoo.desert.zioschema

import io.github.vigoo.desert.{
  BinaryCodec,
  FieldAdded,
  SerializationProperties,
  TypeRegistry,
  evolutionSteps,
  transientConstructor,
  transientField
}
import zio.schema.{DeriveSchema, Schema}
import zio.test.{Spec, TestEnvironment, ZIOSpecDefault}

object TransientSpec extends ZIOSpecDefault with SerializationProperties {
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

  case class Case1(a: Int) extends SumWithTransientCons

  @transientConstructor case class Case2(data: TypeWithoutCodec) extends SumWithTransientCons

  case class Case3(x: String) extends SumWithTransientCons

  private implicit val typeRegistry: TypeRegistry = TypeRegistry.empty

  implicit val ttSchema: Schema[TransientTest]         = DeriveSchema.gen[TransientTest]
  implicit val swtSchema: Schema[SumWithTransientCons] = DeriveSchema.gen[SumWithTransientCons]

  implicit val ttCodec: BinaryCodec[TransientTest]         = DerivedBinaryCodec.derive[TransientTest]
  implicit val swtCodec: BinaryCodec[SumWithTransientCons] = DerivedBinaryCodec.derive[SumWithTransientCons]

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
