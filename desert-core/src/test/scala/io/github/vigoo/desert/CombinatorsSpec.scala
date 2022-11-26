package io.github.vigoo.desert

import zio.Scope
import zio.test.Assertion.equalTo
import zio.test.{Spec, TestEnvironment, ZIOSpecDefault}

object CombinatorsSpec extends ZIOSpecDefault with SerializationProperties {
  override def spec: Spec[TestEnvironment with Scope, Any] =
    suite("BinaryCodec combinators")(
      test("mapOrFail") {
        val codec = stringCodec.mapOrFail {
          case "good"        => Right(1)
          case other: String => Left(DesertFailure.DeserializationFailure(s"invalid: $other", None))
        }

        canBeSerializedAndReadBack("good", 1)(stringCodec, codec, TypeRegistry.empty) &&
        failsWhenReadBack("bad", equalTo(DesertFailure.DeserializationFailure("invalid: bad", None)))(
          stringCodec,
          codec,
          TypeRegistry.empty
        )
      },
      test("contramapOrFail") {
        val codec = intCodec.contramapOrFail[String] {
          case "one"         => Right(1)
          case other: String => Left(DesertFailure.SerializationFailure(s"invalid: $other", None))
        }

        canBeSerializedAndReadBack("one", 1)(codec, intCodec, TypeRegistry.empty) &&
        failsWhenReadBack("two", equalTo(DesertFailure.SerializationFailure("invalid: two", None)))(
          codec,
          intCodec,
          TypeRegistry.empty
        )
      }
    )
}
