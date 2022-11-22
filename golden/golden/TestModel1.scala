// NOTE: cannot be shared by the implementations from desert-core/Test as before because
// of compiler bug https://github.com/lampepfl/dotty/issues/16318
package io.github.vigoo.desert.golden

import io.github.vigoo.desert.golden.TestModel1.{ListElement1, ListElement2}
import io.github.vigoo.desert.{
  FieldAdded,
  FieldMadeOptional,
  FieldMadeTransient,
  PersistedThrowable,
  evolutionSteps,
  transientConstructor,
  transientField
}

import java.util.UUID
import scala.util.{Success, Try}

@evolutionSteps(
  FieldMadeOptional("option"),
  FieldAdded("string", "default string"),
  FieldAdded("set", Set.empty[String])
)
case class TestModel1(
    byte: Byte,
    short: Short,
    int: Int,
    long: Long,
    float: Float,
    double: Double,
    boolean: Boolean,
    unit: Unit,
    string: String,
    uuid: UUID,
    exception: Throwable,
    list: List[ListElement1],
    array: Array[Long],
    vector: Vector[ListElement1],
    set: Set[String],
    either: Either[String, Boolean],
    tried: Try[ListElement2],
    option: Option[Map[String, ListElement2]]
)

object TestModel1 {

  case class ListElement1(id: String)

  sealed trait ListElement2

  object ListElement2 {
    final case class First(elem: ListElement1) extends ListElement2

    @evolutionSteps(FieldMadeTransient("cached"))
    final case class Second(uuid: UUID, desc: Option[String], @transientField(None) cached: Option[String])
        extends ListElement2

    @transientConstructor
    final case class Third(file: java.io.File)
  }

  val value1: TestModel1 =
    TestModel1(
      byte = -10,
      short = 10000,
      int = -2000000000,
      long = 100000000001L,
      float = 3.14f,
      double = 0.1234e-10,
      boolean = false,
      unit = (),
      string = "Example data set",
      uuid = UUID.fromString("d90c4285-544d-424d-885c-3940fe00883d"),
      exception = PersistedThrowable(
        className = "java.lang.RuntimeException",
        message = "Example exception",
        stackTrace = Array(
          new StackTraceElement(
            "io.github.vigoo.desert.golden.TestModel1$",
            "generateException",
            "TestModel1.scala",
            67
          ),
          new StackTraceElement(
            "io.github.vigoo.desert.golden.TestModel1$",
            "<clinit>",
            "TestModel1.scala",
            84
          ),
          new StackTraceElement("io.github.vigoo.desert.golden.Main$", "$anonfun$run$3", "Main.scala", 31),
          new StackTraceElement("zio.ZIO$FlatMap", "apply", "ZIO.scala", 5210),
          new StackTraceElement("zio.ZIO$FlatMap", "apply", "ZIO.scala", 5199),
          new StackTraceElement("zio.internal.FiberContext", "runUntil", "FiberContext.scala", 901),
          new StackTraceElement("zio.internal.FiberContext", "run", "FiberContext.scala", 111),
          new StackTraceElement("zio.Runtime", "unsafeRunWithRefs", "Runtime.scala", 400),
          new StackTraceElement("zio.Runtime", "unsafeRunWith", "Runtime.scala", 355),
          new StackTraceElement("zio.Runtime", "unsafeRunAsyncCancelable", "Runtime.scala", 308),
          new StackTraceElement("zio.Runtime", "unsafeRunAsyncCancelable$", "Runtime.scala", 304),
          new StackTraceElement("zio.Runtime$$anon$2", "unsafeRunAsyncCancelable", "Runtime.scala", 425),
          new StackTraceElement("zio.Runtime", "$anonfun$run$2", "Runtime.scala", 78),
          new StackTraceElement("zio.internal.FiberContext", "runUntil", "FiberContext.scala", 316),
          new StackTraceElement("zio.internal.FiberContext", "run", "FiberContext.scala", 111),
          new StackTraceElement("zio.internal.ZScheduler$$anon$3", "run", "ZScheduler.scala", 415)
        ),
        cause = Some(
          PersistedThrowable(
            className = "java.lang.IllegalArgumentException",
            message = "param should not be negative",
            stackTrace = Array(
              new StackTraceElement(
                "io.github.vigoo.desert.golden.TestModel1$",
                "generateException",
                "TestModel1.scala",
                67
              ),
              new StackTraceElement(
                "io.github.vigoo.desert.golden.TestModel1$",
                "<clinit>",
                "TestModel1.scala",
                84
              ),
              new StackTraceElement("io.github.vigoo.desert.golden.Main$", "$anonfun$run$3", "Main.scala", 31),
              new StackTraceElement("zio.ZIO$FlatMap", "apply", "ZIO.scala", 5210),
              new StackTraceElement("zio.ZIO$FlatMap", "apply", "ZIO.scala", 5199),
              new StackTraceElement("zio.internal.FiberContext", "runUntil", "FiberContext.scala", 901),
              new StackTraceElement("zio.internal.FiberContext", "run", "FiberContext.scala", 111),
              new StackTraceElement("zio.Runtime", "unsafeRunWithRefs", "Runtime.scala", 400),
              new StackTraceElement("zio.Runtime", "unsafeRunWith", "Runtime.scala", 355),
              new StackTraceElement("zio.Runtime", "unsafeRunAsyncCancelable", "Runtime.scala", 308),
              new StackTraceElement("zio.Runtime", "unsafeRunAsyncCancelable$", "Runtime.scala", 304),
              new StackTraceElement("zio.Runtime$$anon$2", "unsafeRunAsyncCancelable", "Runtime.scala", 425),
              new StackTraceElement("zio.Runtime", "$anonfun$run$2", "Runtime.scala", 78),
              new StackTraceElement("zio.internal.FiberContext", "runUntil", "FiberContext.scala", 316),
              new StackTraceElement("zio.internal.FiberContext", "run", "FiberContext.scala", 111),
              new StackTraceElement("zio.internal.ZScheduler$$anon$3", "run", "ZScheduler.scala", 415)
            ),
            cause = None
          )
        )
      ),
      list = List(ListElement1("a"), ListElement1("aa"), ListElement1("aaa")),
      array = (1L to 30000L).toArray,
      vector = (1 to 100).map(n => ListElement1(n.toString)).toVector,
      set = Set("hello", "world"),
      either = Right(true),
      tried = Success(ListElement2.First(ListElement1(""))),
      option = Some(
        Map(
          "first"  -> ListElement2.First(ListElement1("1st")),
          "second" -> ListElement2
            .Second(UUID.fromString("0ca26648-edee-4a2d-bd88-eebf92d19c30"), None, None),
          "third"  -> ListElement2.Second(
            UUID.fromString("0ca26648-edee-4a2d-bd88-eebf92d19c30"),
            Some("some description"),
            None
          )
        )
      )
    )
}
