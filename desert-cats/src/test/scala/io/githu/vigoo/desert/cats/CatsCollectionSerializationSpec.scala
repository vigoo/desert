package io.githu.vigoo.desert.cats

import cats.data.{NonEmptyList, NonEmptyMap, NonEmptySet, Validated}
import cats.instances.string._
import io.github.vigoo.desert.SerializationProperties
import io.github.vigoo.desert.catssupport._
import io.github.vigoo.desert._
import zio.test._

import scala.collection.immutable.{SortedMap, SortedSet}

object CatsCollectionSerializationSpec extends ZIOSpecDefault with SerializationProperties {
  override def spec: Spec[TestEnvironment, Any] =
    suite("Cats collections can be serialized and read back")(
      test("validated")(canBeSerialized(Gen.either(Gen.int, Gen.string).map(Validated.fromEither))),
      test("non-empty list")(canBeSerialized(Gen.listOf1(Gen.string).map(NonEmptyList.fromList))),
      test("non-empty set")(
        canBeSerialized(Gen.setOf1(Gen.string).map(set => NonEmptySet.fromSet(SortedSet.from(set))))
      ),
      test("non-empty map")(
        canBeSerialized(
          Gen.mapOf1(Gen.string, Gen.int).map(map => NonEmptyMap.fromMap(SortedMap.from[String, Int](map)))
        )
      )
    )
}
