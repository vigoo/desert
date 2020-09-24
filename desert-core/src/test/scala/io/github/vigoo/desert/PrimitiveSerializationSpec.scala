package io.github.vigoo.desert

import io.github.vigoo.desert.BinaryDeserializerOps._
import io.github.vigoo.desert.BinarySerializerOps._
import io.github.vigoo.desert.codecs._
import zio.test._

object PrimitiveSerializationSpec extends DefaultRunnableSpec with SerializationProperties {
  override def spec: ZSpec[_root_.zio.test.environment.TestEnvironment, Any] =
    suite("Primitive values can be serialized and read back")(
      testM("boolean")(canBeSerialized(Gen.boolean)),
      testM("byte")(canBeSerialized(Gen.anyByte)),
      testM("short")(canBeSerialized(Gen.anyShort)),
      testM("int")(canBeSerialized(Gen.anyInt)),
      testM("long")(canBeSerialized(Gen.anyLong)),
      testM("float")(canBeSerialized(Gen.anyFloat)),
      testM("double")(canBeSerialized(Gen.anyDouble)),
      testM("string")(canBeSerialized(Gen.anyString)),
      testM("unit")(canBeSerialized(Gen.unit)),
      testM("uuid")(canBeSerialized(Gen.anyUUID)),

      suite("variable length int")(
        testM("optimized for positive") {
          val varIntCodec = BinaryCodec.define(value => writeVarInt(value, optimizeForPositive = true))(readVarInt(optimizeForPositive = true))
          canBeSerialized(Gen.anyInt)(varIntCodec)
        },
        testM("not optimized for positive") {
          val varIntCodec = BinaryCodec.define(value => writeVarInt(value, optimizeForPositive = false))(readVarInt(optimizeForPositive = false))
          canBeSerialized(Gen.anyInt)(varIntCodec)
        }
      )
    )
}
