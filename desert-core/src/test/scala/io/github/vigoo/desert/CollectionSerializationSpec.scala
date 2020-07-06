package io.github.vigoo.desert

import zio.test.environment.TestEnvironment
import io.github.vigoo.desert.codecs._
import org.junit.runner.RunWith
import zio.test._

import scala.annotation.nowarn
import scala.collection.immutable.{SortedMap, SortedSet}
import scala.util.{Failure, Success, Try}

@RunWith(classOf[zio.test.junit.ZTestJUnitRunner])
class CollectionSerializationSpec extends DefaultRunnableSpec with SerializationProperties {
  override def spec: ZSpec[TestEnvironment, Any] =
    suite("Collections can be serialized and read back")(
      testM("array of strings")(canBeSerialized(Gen.listOf(Gen.anyString).map(_.toArray))),
      testM("array of ints")(canBeSerialized(Gen.listOf(Gen.anyInt).map(_.toArray))),
      testM("list of strings")(canBeSerialized(Gen.listOf(Gen.anyString))),
      testM("vector of strings")(canBeSerialized(Gen.vectorOf(Gen.anyString))),
      testM("set of strings")(canBeSerialized(Gen.setOf(Gen.anyString))),
      testM("string -> int map")(canBeSerialized(Gen.mapOf(Gen.anyString, Gen.anyInt))),
      testM("option")(canBeSerialized(Gen.option(Gen.anyString))),
      testM("either")(canBeSerialized(Gen.either(Gen.anyInt, Gen.anyString))),
//      testM("validated")(canBeSerialized(Gen.either(Gen.anyInt, Gen.anyString).map(Validated.fromEither))),
//      testM("non-empty list")(canBeSerialized(Gen.listOf1(Gen.anyString).map(NonEmptyList.fromList))),
//      testM("non-empty set")(canBeSerialized(Gen.setOf1(Gen.anyString).map(set => NonEmptySet.fromSet(SortedSet.from(set))))),
//      testM("non-empty map")(canBeSerialized(Gen.mapOf1(Gen.anyString, Gen.anyInt).map(map => NonEmptyMap.fromMap(SortedMap.from[String, Int](map))))),

      testM("try")(canBeSerialized(Gen.either(Gen.throwable, Gen.anyString).map(_.toTry), Some({ source: Try[String] =>
        import Assertion._

        source match {
          case Failure(exception) =>
            isFailure(isSubtype[PersistedThrowable](anything))
          case Success(value) =>
            isSuccess(equalTo(value))
        }
      }))),
    )
}

@nowarn object CollectionSerializationSpec extends CollectionSerializationSpec