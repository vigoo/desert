package io.github.vigoo.desert

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

import io.github.vigoo.desert.BinaryDeserializer.{Deser, DeserializationEnv}
import io.github.vigoo.desert.codecs._
import io.github.vigoo.desert.BinarySerializerOps._
import io.github.vigoo.desert.BinaryDeserializerOps._
import io.github.vigoo.desert.BinarySerializer.{Ser, SerializationEnv}
import org.junit.runner.RunWith
import zio.ZIO
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
      testM("reads back duplicated strings correctly") {
        for {
          stream <- ZIO.effect(new ByteArrayOutputStream())
          output <- ZIO.effect(new JavaStreamBinaryOutput(stream))
          serEnv <- SerializationEnv.create(output, TypeRegistry.empty)
          _ <- testSer.provide(serEnv)
          _ <- ZIO.effect(stream.flush())
          inStream <- ZIO.effect(new ByteArrayInputStream(stream.toByteArray))
          input <- ZIO.effect(new JavaStreamBinaryInput(inStream))
          deserEnv <- DeserializationEnv.create(input, TypeRegistry.empty)
          result <- testDeser.provide(deserEnv)
        } yield assert(result)(equalTo(List(s1, s2, s3, s1, s2, s3)))
      },
      testM("reduces the serialized size") {
        for {
          stream <- ZIO.effect(new ByteArrayOutputStream())
          output <- ZIO.effect(new JavaStreamBinaryOutput(stream))
          serEnv <- SerializationEnv.create(output, TypeRegistry.empty)
          _ <- testSer.provide(serEnv)
          _ <- ZIO.effect(stream.flush())
          size <- ZIO.effect(stream.toByteArray.length)
        } yield assert(size)(isLessThan((s1.length + s2.length) * 2))
      }
    )
}

@nowarn object StringDeduplicationSpec extends StringDeduplicationSpec