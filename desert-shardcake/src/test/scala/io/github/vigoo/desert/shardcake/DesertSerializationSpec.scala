package io.github.vigoo.desert.shardcake

import com.devsisters.shardcake.interfaces.Serialization
import io.github.vigoo.desert.{BinaryCodec, DefaultTypeRegistry, TypeRegistry}
import io.github.vigoo.desert.zioschema._
import zio._
import zio.schema.{DeriveSchema, Schema}
import zio.test._

object DesertSerializationSpec extends ZIOSpecDefault {

  case class Test(a: Int, b: String)

  object Test {
    implicit val schema: Schema[Test]     = DeriveSchema.gen
    implicit val codec: BinaryCodec[Test] = DerivedBinaryCodec.derive
  }

  def spec: Spec[TestEnvironment with Scope, Any] =
    suite("DesertSerializationSpec")(
      test("serialize back and forth") {

        val expected = Test(2, "test")
        for {
          bytes  <- ZIO.serviceWithZIO[Serialization](_.encode(expected))
          actual <- ZIO.serviceWithZIO[Serialization](_.decode[Test](bytes))
        } yield assertTrue(expected == actual)
      }
    ).provideShared(
      DesertSerialization.withTypeRegistry(
        DefaultTypeRegistry()
          .register[Test]
          .freeze()
      )
    )
}
