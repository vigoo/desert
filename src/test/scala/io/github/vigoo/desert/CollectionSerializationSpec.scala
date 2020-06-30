package io.github.vigoo.desert

import zio.test.environment.TestEnvironment
import io.github.vigoo.desert.codecs._
import org.junit.runner.RunWith
import zio.test._

import scala.annotation.nowarn

@RunWith(classOf[zio.test.junit.ZTestJUnitRunner])
class CollectionSerializationSpec extends DefaultRunnableSpec with SerializationProperties {
  override def spec: ZSpec[TestEnvironment, Any] =
    suite("Collections can be serialized and read back")(
      testM("list of strings")(canBeSerialized(Gen.listOf(Gen.anyString))),
      testM("vector of strings")(canBeSerialized(Gen.vectorOf(Gen.anyString))),
      testM("set of strings")(canBeSerialized(Gen.setOf(Gen.anyString))),
      testM("string -> int map")(canBeSerialized(Gen.mapOf(Gen.anyString, Gen.anyInt))),
    )
}

@nowarn object CollectionSerializationSpec extends CollectionSerializationSpec