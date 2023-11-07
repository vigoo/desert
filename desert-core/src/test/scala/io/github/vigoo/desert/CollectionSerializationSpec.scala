package io.github.vigoo.desert

import zio.test._

import scala.util.{Failure, Success, Try}

object CollectionSerializationSpec extends ZIOSpecDefault with SerializationProperties {
  override def spec: Spec[TestEnvironment, Any] =
    suite("Collections can be serialized and read back")(
      test("array of strings")(canBeSerialized(Gen.listOf(Gen.string).map(_.toArray))),
      test("array of ints")(canBeSerialized(Gen.listOf(Gen.int).map(_.toArray))),
      test("list of strings")(canBeSerialized(Gen.listOf(Gen.string))),
      test("vector of strings")(canBeSerialized(Gen.vectorOf(Gen.string))),
      test("set of strings")(canBeSerialized(Gen.setOf(Gen.string))),
      test("string -> int map")(canBeSerialized(Gen.mapOf(Gen.string, Gen.int))),
      test("option")(canBeSerialized(Gen.option(Gen.string))),
      test("either")(canBeSerialized(Gen.either(Gen.int, Gen.string))),
      test("try")(
        canBeSerialized(
          Gen.either(Gen.throwable, Gen.string).map(_.toTry),
          Some({ (source: Try[String]) =>
            import Assertion._

            source match {
              case Failure(exception) =>
                isFailure(isSubtype[PersistedThrowable](anything))
              case Success(value)     =>
                isSuccess(equalTo(value))
            }
          })
        )
      ),
      test("unknown sized collection serializer is stack safe") {
        implicit val typeRegistry: TypeRegistry = TypeRegistry.empty
        val bigList                             = (0 to 100000).toList
        canBeSerializedAndReadBack(bigList, bigList)
      },
      test("known sized collection serializer is stack safe") {
        implicit val typeRegistry: TypeRegistry = TypeRegistry.empty
        val bigVector                           = (0 to 100000).toVector
        canBeSerializedAndReadBack(bigVector, bigVector)
      }
    )
}
