package io.github.vigoo.desert.catseffect

import io.github.vigoo.desert.catseffect.syntax._
import io.github.vigoo.desert.codecs._
import zio.Task
import zio.interop.catz._
import zio.test.Assertion._
import zio.test._
import zio.test.environment._

object IOWrapperSpec extends DefaultRunnableSpec {
  override def spec: ZSpec[TestEnvironment, Throwable] =
    suite("Cats effect syntax")(
      testM("is an IO wrapper around the core functionality") {
        for {
          bytes <- serializeToArray[Task, String]("Hello world")
          deser <- deserializeFromArray[Task, String](bytes)
        } yield assert(deser)(equalTo("Hello world"))
      }
    )
}
