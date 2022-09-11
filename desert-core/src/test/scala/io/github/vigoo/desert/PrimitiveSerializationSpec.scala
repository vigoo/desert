package io.github.vigoo.desert

import io.github.vigoo.desert.BinaryDeserializerOps._
import io.github.vigoo.desert.BinarySerializerOps._
import io.github.vigoo.desert.codecs._
import zio.test._

object PrimitiveSerializationSpec extends ZIOSpecDefault with SerializationProperties {
  override def spec =
    suite("Primitive values can be serialized and read back")(
      test("boolean")(canBeSerialized(Gen.boolean)),
      test("byte")(canBeSerialized(Gen.byte)),
      test("short")(canBeSerialized(Gen.short)),
      test("int")(canBeSerialized(Gen.int)),
      test("long")(canBeSerialized(Gen.long)),
      test("float")(canBeSerialized(Gen.float)),
      test("double")(canBeSerialized(Gen.double)),
      test("string")(canBeSerialized(Gen.string)),
      test("unit")(canBeSerialized(Gen.unit)),
      test("uuid")(canBeSerialized(Gen.uuid)),
      suite("variable length int")(
        test("optimized for positive") {
          val varIntCodec = BinaryCodec
            .define(value => writeVarInt(value, optimizeForPositive = true))(readVarInt(optimizeForPositive = true))
          canBeSerialized(Gen.int)(varIntCodec)
        },
        test("not optimized for positive") {
          val varIntCodec = BinaryCodec.define(value => writeVarInt(value, optimizeForPositive = false))(
            readVarInt(optimizeForPositive = false)
          )
          canBeSerialized(Gen.int)(varIntCodec)
        }
      )
    )
}
