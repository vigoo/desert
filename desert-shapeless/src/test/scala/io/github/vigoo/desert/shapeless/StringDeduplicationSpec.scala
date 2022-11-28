package io.github.vigoo.desert.shapeless

import io.github.vigoo.desert.Evolution.FieldAdded
import io.github.vigoo.desert._
import io.github.vigoo.desert.custom._
import io.github.vigoo.desert.internal.{
  DeserializationContext,
  DeserializationEnv,
  JavaStreamBinaryInput,
  JavaStreamBinaryOutput,
  SerializationContext,
  SerializationEnv,
  SerializerState
}
import zio.test.Assertion.{equalTo, isLessThan, isRight}
import zio.test.{Spec, TestEnvironment, ZIOSpecDefault, assert, assertTrue}

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

object StringDeduplicationSpec extends ZIOSpecDefault with SerializationProperties {
  case class DataV1()

  @evolutionSteps(FieldAdded[String]("newField", "unknown"))
  case class DataV2(newField: String)

  case class OuterV1(data: DataV1, other: String)

  case class OuterV2(data: DataV2, other: String)

  val s1 = "this is a test string"
  val s2 = "and another one"
  val s3 = "and another one"

  implicit val v1codec: BinaryCodec[DataV1]       = DerivedBinaryCodec.derive
  implicit val v2codec: BinaryCodec[DataV2]       = DerivedBinaryCodec.derive
  implicit val outerv1codec: BinaryCodec[OuterV1] = DerivedBinaryCodec.derive
  implicit val outerv2codec: BinaryCodec[OuterV2] = DerivedBinaryCodec.derive

  private def testSer(implicit ctx: SerializationContext): Unit = {
    write(DeduplicatedString(s1))
    write(DeduplicatedString(s2))
    write(DeduplicatedString(s3))
    write(DeduplicatedString(s1))
    write(DeduplicatedString(s2))
    write(DeduplicatedString(s3))
  }

  private def testDeser(implicit ctx: DeserializationContext): List[String] =
    List(
      read[DeduplicatedString](),
      read[DeduplicatedString](),
      read[DeduplicatedString](),
      read[DeduplicatedString](),
      read[DeduplicatedString](),
      read[DeduplicatedString]()
    ).map(_.string)

  override def spec: Spec[TestEnvironment, Any] =
    suite("String deduplication")(
      test("reads back duplicated strings correctly") {
        val stream                                = new ByteArrayOutputStream()
        val output                                = new JavaStreamBinaryOutput(stream)
        implicit val sctx: SerializationContext   =
          SerializationContext(SerializationEnv(output, TypeRegistry.empty), SerializerState.create)
        testSer
        stream.flush()
        val inStream                              = new ByteArrayInputStream(stream.toByteArray)
        val input                                 = new JavaStreamBinaryInput(inStream)
        implicit val dctx: DeserializationContext =
          DeserializationContext(DeserializationEnv(input, TypeRegistry.empty), SerializerState.create)
        val result                                = testDeser

        assertTrue(result == List(s1, s2, s3, s1, s2, s3))
      },
      test("reduces the serialized size") {
        val stream                              = new ByteArrayOutputStream()
        val output                              = new JavaStreamBinaryOutput(stream)
        implicit val sctx: SerializationContext =
          SerializationContext(SerializationEnv(output, TypeRegistry.empty), SerializerState.create)
        testSer
        val size                                = stream.toByteArray.length

        assertTrue(size < (s1.length + s2.length) * 2)
      },
      test("default string serialization does not breaks data evolution") {
        implicit val typeRegistry: TypeRegistry = TypeRegistry.empty
        canBeSerializedAndReadBack(
          OuterV2(DataV2("hello world"), "hello world"),
          OuterV1(DataV1(), "hello world")
        )
      }
    )
}
