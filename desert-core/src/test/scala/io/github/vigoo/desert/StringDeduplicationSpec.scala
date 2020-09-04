package io.github.vigoo.desert
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

import io.github.vigoo.desert.BinaryDeserializer.{Deser, DeserializationEnv}
import io.github.vigoo.desert.codecs._
import io.github.vigoo.desert.BinarySerializerOps._
import io.github.vigoo.desert.BinaryDeserializerOps._
import io.github.vigoo.desert.BinarySerializer.{Ser, SerializationEnv}
import org.junit.runner.RunWith
import zio.test._
import zio.test.environment.TestEnvironment
import zio.test.Assertion._

import scala.annotation.nowarn

@RunWith(classOf[zio.test.junit.ZTestJUnitRunner])
class StringDeduplicationSpec extends DefaultRunnableSpec with SerializationProperties {

  val s1 = "this is a test string"
  val s2 = "and another one"
  val s3 = "and another one"

  case class DataV1()
  object DataV1 {
    implicit val codec: BinaryCodec[DataV1] = BinaryCodec.derive()
  }

  case class DataV2(newField: String)
  object DataV2 {
    implicit val codec: BinaryCodec[DataV2] = BinaryCodec.derive(FieldAdded[String]("newField", "unknown"))
  }

  case class OuterV1(data: DataV1, other: String)
  object OuterV1 {
    implicit val codec: BinaryCodec[OuterV1] = BinaryCodec.derive()
  }

  case class OuterV2(data: DataV2, other: String)
  object OuterV2 {
    implicit val codec: BinaryCodec[OuterV2] = BinaryCodec.derive()
  }

  private val testSer: Ser[Unit] = for {
    _ <- write(DeduplicatedString(s1))
    _ <- write(DeduplicatedString(s2))
    _ <- write(DeduplicatedString(s3))
    _ <- write(DeduplicatedString(s1))
    _ <- write(DeduplicatedString(s2))
    _ <- write(DeduplicatedString(s3))
  } yield ()

  private val testDeser: Deser[List[String]] =
    for {
      a <- read[DeduplicatedString]()
      b <- read[DeduplicatedString]()
      c <- read[DeduplicatedString]()
      d <- read[DeduplicatedString]()
      e <- read[DeduplicatedString]()
      f <- read[DeduplicatedString]()
    } yield List(a, b, c, d, e, f).map(_.string)

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("String deduplication")(
      test("reads back duplicated strings correctly") {
        val stream = new ByteArrayOutputStream()
        val output = new JavaStreamBinaryOutput(stream)
        val result = testSer.run(SerializationEnv(output, TypeRegistry.empty)).runA(SerializerState.initial).value.flatMap { _ =>
          stream.flush()
          val inStream = new ByteArrayInputStream(stream.toByteArray)
          val input = new JavaStreamBinaryInput(inStream)
          testDeser.run(DeserializationEnv(input, TypeRegistry.empty)).runA(SerializerState.initial).value
        }.value

        assert(result)(isRight(equalTo(List(s1, s2, s3, s1, s2, s3))))
      },
      test("reduces the serialized size") {
        val stream = new ByteArrayOutputStream()
        val output = new JavaStreamBinaryOutput(stream)
        val size = testSer.run(SerializationEnv(output, TypeRegistry.empty)).runA(SerializerState.initial).map { _ =>
          stream.flush()
          stream.toByteArray.length
        }.value.value

        assert(size)(isRight(isLessThan((s1.length + s2.length) * 2)))
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

@nowarn object StringDeduplicationSpec extends StringDeduplicationSpec