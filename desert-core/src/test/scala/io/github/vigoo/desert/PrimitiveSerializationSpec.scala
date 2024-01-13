package io.github.vigoo.desert

import zio.Scope
import zio.test.Assertion.equalTo
import zio.test._

object PrimitiveSerializationSpec extends ZIOSpecDefault with SerializationProperties {
  override def spec: Spec[TestEnvironment with Scope, Any] =
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
      test("char")(canBeSerialized(Gen.char)),
      suite("variable length int")(
        test("optimized for positive") {
          canBeSerialized(Gen.int)(varIntOptimizedForPositiveCodec)
        },
        test("not optimized for positive") {
          canBeSerialized(Gen.int)(varIntCodec)
        }
      ),
      test("bigDecimal")(
        canBeSerialized(
          Gen.bigDecimal(
            BigDecimal(Double.MinValue) * BigDecimal(Double.MaxValue),
            BigDecimal(Double.MaxValue) * BigDecimal(Double.MaxValue)
          )
        )
      ),
      test("javaBigDecimal")(
        canBeSerialized(
          Gen.bigDecimalJava(
            BigDecimal(Double.MinValue) * BigDecimal(Double.MaxValue),
            BigDecimal(Double.MaxValue) * BigDecimal(Double.MaxValue)
          )
        )
      ),
      test("bigInt")(
        canBeSerialized(
          Gen.bigInt(
            BigInt(Int.MinValue) * BigInt(Int.MaxValue),
            BigInt(Int.MaxValue) * BigInt(Int.MaxValue)
          )
        )
      ),
      test("javaBigInteger")(
        canBeSerialized(
          Gen.bigIntegerJava(
            BigInt(Int.MinValue) * BigInt(Int.MaxValue),
            BigInt(Int.MaxValue) * BigInt(Int.MaxValue)
          )
        )
      ),
      test("dayOfWeek")(canBeSerialized(Gen.dayOfWeek)),
      test("month")(canBeSerialized(Gen.month)),
      test("year")(canBeSerialized(Gen.year)),
      test("monthDay")(canBeSerialized(Gen.monthDay)),
      test("yearMonth")(canBeSerialized(Gen.yearMonth)),
      test("period")(canBeSerialized(Gen.period)),
      test("zoneId")(canBeSerialized(Gen.zoneId)),
      test("zoneOffset")(canBeSerialized(Gen.zoneOffset)),
      test("duration")(canBeSerialized(Gen.finiteDuration)),
      test("instant")(canBeSerialized(Gen.instant)),
      test("localDate")(canBeSerialized(Gen.localDate)),
      test("localTime")(canBeSerialized(Gen.localTime)),
      test("localDateTime")(canBeSerialized(Gen.localDateTime)),
      test("offsetTime")(canBeSerialized(Gen.offsetTime)),
      test("offsetDateTime")(canBeSerialized(Gen.offsetDateTime)),
      test("zonedDateTime")(canBeSerialized(Gen.zonedDateTime)),
      test("debug") {
        val value        = -1
        val serialized   = serializeToArray(value)(varIntCodec)
        println(serialized.map(_.toList.map(_.toHexString)))
        val deserialized = deserializeFromArray(serialized.toOption.get)(varIntCodec)
        println(deserialized)
        assertCompletes
      }
    )
}
