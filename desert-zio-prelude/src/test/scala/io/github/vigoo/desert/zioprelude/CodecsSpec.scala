package io.github.vigoo.desert.zioprelude

import io.github.vigoo.desert._
import zio._
import zio.prelude._
import zio.test.{Gen, Spec, TestEnvironment, ZIOSpecDefault}

object CodecsSpec extends ZIOSpecDefault with SerializationProperties {
  override def spec: Spec[TestEnvironment with Scope, Any] =
    suite("ZIO Prelude types can be serialized and read back")(
      test("validation")(
        canBeSerialized(
          Gen
            .either(Gen.listOf1(Gen.int).map(l => NonEmptyChunk.fromIterable(l.head, l.tail)), Gen.string)
            .map(Validation.fromEither)
        )
      ),
      test("non-empty list")(
        canBeSerialized(Gen.listOf1(Gen.string).map(l => NonEmptyList.fromIterable(l.head, l.tail)))
      ),
      test("zset")(canBeSerialized(Gen.mapOf(Gen.string, Gen.int).map(ZSet.fromMap)))
    )
}
