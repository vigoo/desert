package io.github.vigoo.desert

import zio.test.environment.TestEnvironment
import io.github.vigoo.desert.codecs._
import org.junit.runner.RunWith
import zio.NonEmptyChunk
import zio.prelude.{NonEmptyList, Validation, ZSet}
import zio.test._
import zio.test.environment.TestEnvironment

import scala.collection.immutable.{SortedMap, SortedSet}
import scala.util.{Failure, Success, Try}

object CollectionSerializationSpec extends DefaultRunnableSpec with SerializationProperties {
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
      testM("validation")(canBeSerialized(Gen.either(Gen.listOf1(Gen.anyInt).map(l => NonEmptyChunk.fromIterable(l.head, l.tail)), Gen.anyString).map(Validation.fromEither))),
      testM("non-empty list")(canBeSerialized(Gen.listOf1(Gen.anyString).map(l => NonEmptyList.fromIterable(l.head, l.tail)))),
      testM("zset")(canBeSerialized(Gen.mapOf(Gen.anyString, Gen.anyInt).map(ZSet.fromMap))),

      testM("try")(canBeSerialized(Gen.either(Gen.throwable, Gen.anyString).map(_.toTry), Some({ source: Try[String] =>
        import Assertion._

        source match {
          case Failure(exception) =>
            isFailure(isSubtype[PersistedThrowable](anything))
          case Success(value) =>
            isSuccess(equalTo(value))
        }
      }))),

      test("unknown sized collection serializer is stack safe") {
        implicit val typeRegistry: TypeRegistry = TypeRegistry.empty
        val bigList = (0 to 100000).toList
        canBeSerializedAndReadBack(bigList, bigList)
      },
      test("known sized collection serializer is stack safe") {
        implicit val typeRegistry: TypeRegistry = TypeRegistry.empty
        val bigVector = (0 to 100000).toVector
        canBeSerializedAndReadBack(bigVector, bigVector)
      },
    )
}
