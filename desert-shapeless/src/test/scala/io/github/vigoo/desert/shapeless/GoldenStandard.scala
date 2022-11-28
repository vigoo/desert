package io.github.vigoo.desert.shapeless

import io.github.vigoo.desert._
import io.github.vigoo.desert.golden._
import zio.ZIO
import zio.stream.ZStream
import zio.test.{Spec, TestAspect, TestEnvironment, ZIOSpecDefault, assertTrue}

import java.io.File

object GoldenStandard extends ZIOSpecDefault {
  implicit val typeRegistry: TypeRegistry = TypeRegistry.empty

  private implicit val listElement1Codec: BinaryCodec[TestModel1.ListElement1]              = DerivedBinaryCodec.derive
  private implicit val listElement2FirstCodec: BinaryCodec[TestModel1.ListElement2.First]   = DerivedBinaryCodec.derive
  private implicit val listElement2SecondCodec: BinaryCodec[TestModel1.ListElement2.Second] = DerivedBinaryCodec.derive
  private implicit val listElement2Codec: BinaryCodec[TestModel1.ListElement2]              = DerivedBinaryCodec.derive

  implicit val testModel1Codec: BinaryCodec[TestModel1] = DerivedBinaryCodec.derive

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
