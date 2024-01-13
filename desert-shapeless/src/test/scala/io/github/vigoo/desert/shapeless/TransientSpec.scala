package io.github.vigoo.desert.shapeless

import io.github.vigoo.desert.Evolution.FieldAdded
import io.github.vigoo.desert._
import zio.Chunk
import zio.test.{Spec, TestEnvironment, ZIOSpecDefault, assertTrue}

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

  @evolutionSteps(
    Evolution.FieldAdded("x", 0),
    Evolution.FieldRemoved("z")
  )
  case class Point(x: Int, y: Int, @transientField(None) cachedStr: Option[String])

  private implicit val typeRegistry: TypeRegistry = TypeRegistry.empty

  implicit val ttCodec: BinaryCodec[TransientTest]         = DerivedBinaryCodec.derive
  implicit val case1Codec: BinaryCodec[Case1]              = DerivedBinaryCodec.derive
  implicit val case3Codec: BinaryCodec[Case3]              = DerivedBinaryCodec.derive
  implicit val swtCodec: BinaryCodec[SumWithTransientCons] = DerivedBinaryCodec.derive
  implicit val pointCodec: BinaryCodec[Point]              = DerivedBinaryCodec.derive

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
      },
      test("expected binary format") {
        val point                 = Point(1, -10, None)
        val bytes                 = serializeToArray(point).map(Chunk.fromArray)
        val expected: Chunk[Byte] = Chunk(2, 8, 8, 3, 2, 122, -1, -1, -1, -10, 0, 0, 0, 1)
        assertTrue(bytes.toOption.get == expected)
      }
    )
}
