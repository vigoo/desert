package io.github.vigoo.desert.catseffect

import cats.effect.IO
import io.github.vigoo.desert.catseffect.syntax._
import io.github.vigoo.desert.codecs._
import zio.ZIO
import zio.test.Assertion._
import zio.test._

object IOWrapperSpec extends ZIOSpecDefault {
  override def spec: ZSpec[TestEnvironment, Throwable] =
    suite("Cats effect syntax")(
      test("is an IO wrapper around the core functionality") {
        ZIO.attempt {
          val test = for {
            bytes <- serializeToArray[IO, String]("Hello world")
            deser <- deserializeFromArray[IO, String](bytes)
          } yield assert(deser)(equalTo("Hello world"))

          import cats.effect.unsafe.implicits.global
          test.unsafeRunSync()
        }
      }
    )
}
