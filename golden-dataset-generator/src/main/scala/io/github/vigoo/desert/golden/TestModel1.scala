package io.github.vigoo.desert.golden

import io.github.vigoo.desert.{
  BinaryCodec,
  FieldAdded,
  FieldMadeOptional,
  FieldMadeTransient,
  TransientConstructor,
  TransientField
}
import io.github.vigoo.desert.codecs._
import io.github.vigoo.desert.golden.TestModel1.{ListElement1, ListElement2}

import java.util.UUID
import scala.util.{Success, Try}

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

  object ListElement1 {
    implicit val codec: BinaryCodec[ListElement1] = BinaryCodec.derive()
  }

  sealed trait ListElement2

  object ListElement2 {
    final case class First(elem: ListElement1) extends ListElement2

    object First {
      implicit val codec: BinaryCodec[First] = BinaryCodec.derive()
    }

    final case class Second(uuid: UUID, desc: Option[String], @TransientField(None) cached: Option[String])
        extends ListElement2

    object Second {
      implicit val codec: BinaryCodec[Second] = BinaryCodec.derive(FieldMadeTransient("cached"))
    }

    @TransientConstructor
    final case class Third(file: java.io.File)

    implicit val codec: BinaryCodec[ListElement2] = BinaryCodec.derive()
  }

  implicit val codec: BinaryCodec[TestModel1] = BinaryCodec.derive(
    FieldMadeOptional("option"),
    FieldAdded("string", "default string"),
    FieldAdded("set", Set.empty[String])
  )

  private def generateException =
    try
      throw new RuntimeException("Example exception", new IllegalArgumentException("param should not be negative"))
    catch {
      case ex: Exception => ex
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
      exception = generateException,
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
            .Second(UUID.fromString("0ca26648-edee-4a2d-bd88-eebf92d19c30"), None, Some("cached")),
          "third"  -> ListElement2.Second(
            UUID.fromString("0ca26648-edee-4a2d-bd88-eebf92d19c30"),
            Some("some description"),
            None
          )
        )
      )
    )
}
