package io.github.vigoo.desert

import io.github.vigoo.desert.codecs._
import zio.Scope
import zio.test.Assertion.equalTo
import zio.test.{Spec, TestEnvironment, ZIOSpecDefault}

object CombinatorsSpec extends ZIOSpecDefault with SerializationProperties {
  override def spec: Spec[TestEnvironment with Scope, Any] =
    suite("BinaryCodec combinators")(
      test("mapOrFail") {
        val codec = stringCodec.mapOrFail {
          case "good"        => Right(1)
          case other: String => Left(DeserializationFailure(s"invalid: $other", None))
        }

        canBeSerializedAndReadBack("good", 1)(stringCodec, codec, TypeRegistry.empty) &&
        failsWhenReadBack("bad", equalTo(DeserializationFailure("invalid: bad", None)))(
          stringCodec,
          codec,
          TypeRegistry.empty
        )
      },
      test("contramapOrFail") {
        val codec = intCodec.contramapOrFail[String] {
          case "one"         => Right(1)
          case other: String => Left(SerializationFailure(s"invalid: $other", None))
        }

        canBeSerializedAndReadBack("one", 1)(codec, intCodec, TypeRegistry.empty) &&
        failsWhenReadBack("two", equalTo(SerializationFailure("invalid: two", None)))(
          codec,
          intCodec,
          TypeRegistry.empty
        )
      }
    )
}
