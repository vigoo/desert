package io.githu.vigoo.desert.cats

import cats.data.{NonEmptyList, NonEmptyMap, NonEmptySet, Validated}
import cats.instances.string._
import io.github.vigoo.desert.SerializationProperties
import io.github.vigoo.desert.cats.codecs._
import io.github.vigoo.desert.codecs._
import zio.test._
import zio.test.environment.TestEnvironment

import scala.collection.immutable.{SortedMap, SortedSet}

object CatsCollectionSerializationSpec extends DefaultRunnableSpec with SerializationProperties {
  override def spec: ZSpec[TestEnvironment, Any] =
    suite("Cats collections can be serialized and read back")(
      testM("validated")(canBeSerialized(Gen.either(Gen.anyInt, Gen.anyString).map(Validated.fromEither))),
      testM("non-empty list")(canBeSerialized(Gen.listOf1(Gen.anyString).map(NonEmptyList.fromList))),
      testM("non-empty set")(canBeSerialized(Gen.setOf1(Gen.anyString).map(set => NonEmptySet.fromSet(SortedSet.from(set))))),
      testM("non-empty map")(canBeSerialized(Gen.mapOf1(Gen.anyString, Gen.anyInt).map(map => NonEmptyMap.fromMap(SortedMap.from[String, Int](map))))),
    )
}
