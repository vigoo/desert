package io.github.vigoo.desert

import io.github.vigoo.desert.golden.TestModel1
import io.github.vigoo.desert.syntax._
import zio.ZIO
import zio.stream.ZStream
import zio.test.{Spec, TestAspect, TestEnvironment, ZIOSpecDefault, assertTrue}

import java.io.File

trait GoldenStandardBase extends ZIOSpecDefault {

  implicit val typeRegistry: TypeRegistry = TypeRegistry.empty
  implicit val testModel1Codec: BinaryCodec[TestModel1]

  override def spec: Spec[TestEnvironment, Any] = suite("GoldenStandard")(
    test("dataset1") {
      for {
        bytes <- ZStream.fromFile(new File("golden/dataset1.bin")).runCollect
        value <- ZIO.fromEither(deserializeFromArray[TestModel1](bytes.toArray, typeRegistry))
      } yield assertTrue(
        value.byte == TestModel1.value1.byte,
        value.short == TestModel1.value1.short,
        value.int == TestModel1.value1.int,
        value.long == TestModel1.value1.long,
        value.float == TestModel1.value1.float,
        value.double == TestModel1.value1.double,
        value.boolean == TestModel1.value1.boolean,
        value.unit == TestModel1.value1.unit,
        value.string == TestModel1.value1.string,
        value.uuid == TestModel1.value1.uuid,
        value.exception.getMessage == TestModel1.value1.exception.getMessage,
        value.exception.getStackTrace.toList == TestModel1.value1.exception.getStackTrace.toList,
        value.exception.getCause.getMessage == TestModel1.value1.exception.getCause.getMessage,
        value.exception.getCause.getStackTrace.toList == TestModel1.value1.exception.getCause.getStackTrace.toList,
        value.exception.getCause.getCause == null,
        value.list == TestModel1.value1.list,
        value.array.toSeq == TestModel1.value1.array.toSeq,
        value.vector == TestModel1.value1.vector,
        value.set == TestModel1.value1.set,
        value.either == TestModel1.value1.either,
        value.tried == TestModel1.value1.tried,
        value.option == TestModel1.value1.option
      )
    }
  ) @@ TestAspect.jvmOnly
}
