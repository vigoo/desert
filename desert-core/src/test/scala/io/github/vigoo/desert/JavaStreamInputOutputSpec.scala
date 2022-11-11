package io.github.vigoo.desert

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

import zio.test.Assertion._
import zio.test._

object JavaStreamInputOutputSpec extends ZIOSpecDefault {
  override def spec: Spec[TestEnvironment, Any] =
    suite("JavaStream input/output")(
      test("properly writes and reads back variable int")(
        check(Gen.int, Gen.boolean) { (value, optForPositive) =>
          testWriteAndRead(
            _.writeVarInt(value, optForPositive),
            _.readVarInt(optForPositive),
            value
          )
        }
      ),
      test("array slice support works") {
        val data = Array[Byte](0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
        testWriteAndRead(
          _.writeBytes(data, 2, 4),
          _.readBytes(4),
          Array[Byte](2, 3, 4, 5)
        )
      },
      test("compressed byte array support")(
        Sized.withSize(500)(
          check(Gen.vectorOf(Gen.byte)) { bytes =>
            testWriteAndRead(
              _.writeCompressedByteArray(bytes.toArray),
              _.readCompressedByteArray().map(_.toVector),
              bytes
            )
          }
        )
      )
    )

  private def testWriteAndRead[T](
      write: BinaryOutput => Unit,
      read: BinaryInput => Either[DesertFailure, T],
      expected: T
  ): TestResult = {
    val outStream = new ByteArrayOutputStream()
    val output    = new JavaStreamBinaryOutput(outStream)
    write(output)
    outStream.flush()
    val data      = outStream.toByteArray

    val inStream  = new ByteArrayInputStream(data)
    val input     = new JavaStreamBinaryInput(inStream)
    val readValue = read(input)

    assert(readValue)(isRight(equalTo(expected)))
  }
}
