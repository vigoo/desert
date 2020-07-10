package io.github.vigoo.desert

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

import org.junit.runner.RunWith
import zio.test._
import zio.test.Assertion._
import zio.test.environment.TestEnvironment

@RunWith(classOf[zio.test.junit.ZTestJUnitRunner])
class JavaStreamInputOutputSpec extends DefaultRunnableSpec {
  override def spec: ZSpec[TestEnvironment, Any] =
    suite("JavaStream input/output")(
      testM("properly writes and reads back variable int")(
        check(Gen.anyInt, Gen.boolean) { (value, optForPositive) =>
          val outStream = new ByteArrayOutputStream()
          val output = new JavaStreamBinaryOutput(outStream)
          output.writeVarInt(value, optForPositive)
          outStream.flush()
          val data = outStream.toByteArray

          val inStream = new ByteArrayInputStream(data)
          val input = new JavaStreamBinaryInput(inStream)
          val readValue = input.readVarInt(optForPositive)

          assert(readValue)(isRight(equalTo(value)))
        }
      )
    )
}

object JavaStreamInputOutputSpec extends JavaStreamInputOutputSpec