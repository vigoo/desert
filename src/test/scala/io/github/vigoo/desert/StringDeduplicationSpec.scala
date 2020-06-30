package io.github.vigoo.desert

import cats.instances.either._
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

import io.github.vigoo.desert.BinaryDeserializer.Deser
import io.github.vigoo.desert.codecs._
import io.github.vigoo.desert.BinarySerializerOps._
import io.github.vigoo.desert.BinaryDeserializerOps._
import io.github.vigoo.desert.BinarySerializer.Ser
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

  private val testSer: Ser[Unit] = for {
    _ <- write(s1)
    _ <- write(s2)
    _ <- write(s3)
    _ <- write(s1)
    _ <- write(s2)
    _ <- write(s3)
  } yield ()

  private val testDeser: Deser[List[String]] =
    for {
      a <- read[String]()
      b <- read[String]()
      c <- read[String]()
      d <- read[String]()
      e <- read[String]()
      f <- read[String]()
    } yield List(a, b, c, d, e, f)

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("String deduplication")(
      test("reads back duplicated strings correctly") {
        val stream = new ByteArrayOutputStream()
        val output = new JavaStreamBinaryOutput(stream)
        val result = testSer.run(output).runA(SerializerState.initial).flatMap { _ =>
          stream.flush()
          val inStream = new ByteArrayInputStream(stream.toByteArray)
          val input = new JavaStreamBinaryInput(inStream)
          testDeser.run(input).runA(SerializerState.initial)
        }

        assert(result)(isRight(equalTo(List(s1, s2, s3, s1, s2, s3))))
      },
      test("reduces the serialized size") {
        val stream = new ByteArrayOutputStream()
        val output = new JavaStreamBinaryOutput(stream)
        val size = testSer.run(output).runA(SerializerState.initial).map { _ =>
          stream.flush()
          stream.toByteArray.length
        }

        assert(size)(isRight(isLessThan((s1.length + s2.length) * 2)))
      }
    )
}

@nowarn object StringDeduplicationSpec extends StringDeduplicationSpec