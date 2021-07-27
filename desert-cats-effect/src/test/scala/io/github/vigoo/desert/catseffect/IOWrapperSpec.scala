package io.github.vigoo.desert.catseffect

import cats.effect.IO
import io.github.vigoo.desert.catseffect.syntax._
import io.github.vigoo.desert.codecs._
import zio.ZIO
import zio.test.Assertion._
import zio.test._
import zio.test.environment._

object IOWrapperSpec extends DefaultRunnableSpec {
  override def spec: ZSpec[TestEnvironment, Throwable] =
    suite("Cats effect syntax")(
      testM("is an IO wrapper around the core functionality") {
        ZIO.effect {
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
