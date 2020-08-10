package io.githu.vigoo.desert.cats

import cats.data.{NonEmptyList, NonEmptyMap, NonEmptySet, Validated}
import cats.instances.string._
import io.github.vigoo.desert.SerializationProperties
import zio.test.environment.TestEnvironment
import io.github.vigoo.desert.codecs._
import io.github.vigoo.desert.cats.codecs._
import org.junit.runner.RunWith
import zio.test._

import scala.annotation.nowarn
import scala.collection.immutable.{SortedMap, SortedSet}
import scala.util.{Failure, Success, Try}

@RunWith(classOf[zio.test.junit.ZTestJUnitRunner])
class CatsCollectionSerializationSpec extends DefaultRunnableSpec with SerializationProperties {
  override def spec: ZSpec[TestEnvironment, Any] =
    suite("Cats collections can be serialized and read back")(
      testM("validated")(canBeSerialized(Gen.either(Gen.anyInt, Gen.anyString).map(Validated.fromEither))),
      testM("non-empty list")(canBeSerialized(Gen.listOf1(Gen.anyString).map(NonEmptyList.fromList))),
      testM("non-empty set")(canBeSerialized(Gen.setOf1(Gen.anyString).map(set => NonEmptySet.fromSet(SortedSet.from(set))))),
      testM("non-empty map")(canBeSerialized(Gen.mapOf1(Gen.anyString, Gen.anyInt).map(map => NonEmptyMap.fromMap(SortedMap.from[String, Int](map))))),
    )
}

@nowarn object CatsCollectionSerializationSpec extends CatsCollectionSerializationSpec