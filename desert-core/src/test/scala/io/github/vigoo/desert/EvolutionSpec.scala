package io.github.vigoo.desert

import codecs._
import org.junit.runner.RunWith
import zio.random.Random
import zio.test.environment.TestEnvironment
import zio.test._
import zio.test.Assertion._
import zio.test.magnolia.DeriveGen

import scala.annotation.nowarn

@RunWith(classOf[zio.test.junit.ZTestJUnitRunner])
class EvolutionSpec extends DefaultRunnableSpec with SerializationProperties {
  implicit val typeRegistry: TypeRegistry = TypeRegistry.empty

  case class ProdV1(fieldA: String, fieldB: Int)

  object ProdV1 {
    implicit val codec: BinaryCodec[ProdV1] = BinaryCodec.derive()
    val gen: Gen[Random with Sized, ProdV1] = DeriveGen[ProdV1]
  }

  case class ProdV2(fieldA: String, newField1: Boolean, fieldB: Int)

  object ProdV2 {
    implicit val codec: BinaryCodec[ProdV2] = BinaryCodec.derive(
      FieldAdded("newField1", true)
    )
    val gen: Gen[Random with Sized, ProdV2] = DeriveGen[ProdV2]
  }

  case class ProdV3(fieldA: String, newField1: Boolean, fieldB: Option[Int])

  object ProdV3 {
    implicit val codec: BinaryCodec[ProdV3] = BinaryCodec.derive(
      FieldAdded("newField1", true),
      FieldMadeOptional("fieldB")
    )
    val gen: Gen[Random with Sized, ProdV3] = DeriveGen[ProdV3]
  }

  case class ProdV4(fieldA: String, newField1: Boolean)

  object ProdV4 {
    implicit val codec: BinaryCodec[ProdV4] = BinaryCodec.derive(
      FieldAdded("newField1", true),
      FieldMadeOptional("fieldB"),
      FieldRemoved("fieldB")
    )
    val gen: Gen[Random with Sized, ProdV4] = DeriveGen[ProdV4]
  }

  case class ProdV5(@TransientField("unset") fieldA: String,
                    newField1: Boolean)

  object ProdV5 {
    implicit val codec: BinaryCodec[ProdV5] = BinaryCodec.derive(
      FieldAdded("newField1", true),
      FieldMadeOptional("fieldB"),
      FieldRemoved("fieldB"),
      FieldMadeTransient("fieldA")
    )
    val gen: Gen[Random with Sized, ProdV5] = DeriveGen[ProdV5]
  }

  import EvolutionSpec.TestId

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("Evolution")(
      suite("tuples vs products")(
        test("tuple can be read as case class")(
          canBeSerializedAndReadBack(
            ("hello", 100),
            ProdV1("hello", 100)
          )
        ),
        test("simple case class can be read as tuple")(
          canBeSerializedAndReadBack(
            ProdV1("hello", 100),
            ("hello", 100)
          )
        ),
      ),
      suite("value type wrappers")(
        test("string can be read as a wrapped string")(
          canBeSerializedAndReadBack(
            "hello world",
            TestId("hello world")
          )
        )
      ),
      suite("collections")(
        test("list to vector")(
          canBeSerializedAndReadBack(
            List(1, 2, 3, 4, 5),
            Vector(1, 2, 3, 4, 5)
          )
        ),
        test("vector to list")(
          canBeSerializedAndReadBack(
            Vector(1, 2, 3, 4, 5),
              List(1, 2, 3, 4, 5)
          )
        ),
        test("list to set")(
          canBeSerializedAndReadBack(
            List(1, 2, 3, 4, 5, 4, 3, 2, 1),
            Set(1, 2, 3, 4, 5)
          )
        ),
        test("set to vector")(
          canBeSerializedAndReadBack[Set[Int], Vector[Int]](
            Set(1, 2, 3, 4, 5),
            hasSameElements(Vector(1, 2, 3, 4, 5))
          )
        ),
      ),
      suite("adding new field")(
        testM("product with added field is serializable")(
          canBeSerialized(ProdV2.gen)
        ),
        test("old version can read new")(
          canBeSerializedAndReadBack(
            ProdV2("hello", newField1 = true, 100),
            ProdV1("hello", 100)
          )
        ),
        test("new version can read old")(
          canBeSerializedAndReadBack(
            ProdV1("hello", 100),
            ProdV2("hello", newField1 = true, 100),
          )
        )
      ),
      suite("making a field optional")(
        testM("product with field made optional is serializable")(
          canBeSerialized(ProdV3.gen)
        ),
        test("v1 version can read new if it is not None")(
          canBeSerializedAndReadBack(
            ProdV3("hello", newField1 = true, Some(200)),
            ProdV1("hello", 200)
          )
        ),
        test("v2 version can read new if it is not None")(
          canBeSerializedAndReadBack(
            ProdV3("hello", newField1 = false, Some(200)),
            ProdV2("hello", newField1 = false, 200)
          )
        ),
        test("old version cannot read new if it is None")(
          cannotBeSerializedAndReadBack[ProdV3, ProdV2](
            ProdV3("hello", newField1 = false, None)
          )
        ),
        test("new version can read v1")(
          canBeSerializedAndReadBack(
            ProdV1("hello", 200),
            ProdV3("hello", newField1 = true, Some(200)),
          )
        ),
        test("new version can read v2")(
          canBeSerializedAndReadBack(
            ProdV2("hello", newField1 = false, 200),
            ProdV3("hello", newField1 = false, Some(200)),
          )
        ),
      ),
      suite("removing a field")(
        testM("product with field removed is serializable")(
          canBeSerialized(ProdV4.gen)
        ),
        test("can read v1 value by skipping the field")(
          canBeSerializedAndReadBack(
            ProdV1("hello", 200),
            ProdV4("hello", newField1 = true)
          )
        ),
        test("can read v2 value by skipping the field")(
          canBeSerializedAndReadBack(
            ProdV2("hello", newField1 = false, 200),
            ProdV4("hello", newField1 = false)
          )
        ),
        test("can read v3 value by skipping the field")(
          canBeSerializedAndReadBack(
            ProdV3("hello", newField1 = false, Some(200)),
            ProdV4("hello", newField1 = false)
          )
        ),
        test("cannot read as v1 because of missing field")(
          cannotBeSerializedAndReadBack[ProdV4, ProdV1](
            ProdV4("hello", newField1 = false),
          )
        ),
        test("cannot read as v2 because of missing field")(
          cannotBeSerializedAndReadBack[ProdV4, ProdV2](
            ProdV4("hello", newField1 = false),
          )
        ),
        test("can read as v3, missing field becomes None")(
          canBeSerializedAndReadBack(
            ProdV4("hello", newField1 = false),
            ProdV3("hello", newField1 = false, fieldB = None),
          )
        )
      ),
      suite("making a field transient")(
        testM("product with field made transient is serializable")(
          canBeSerialized(ProdV5.gen,
            Some((a: ProdV5) =>
              hasField[ProdV5, Boolean]("newField1", _.newField1, equalTo(a.newField1)) &&
              hasField[ProdV5, String]("fieldA", _.fieldA, equalTo("unset"))))
        ),
        test("can read v1 value by skipping the field and using the provided default")(
          canBeSerializedAndReadBack(
            ProdV1("hello", 200),
            ProdV5("unset", newField1 = true)
          )
        ),
        test("can read v2 value by skipping the field and using the provided default")(
          canBeSerializedAndReadBack(
            ProdV2("hello", newField1 = false, 200),
            ProdV5("unset", newField1 = false)
          )
        ),
        test("can read v3 value by skipping the field and using the provided default")(
          canBeSerializedAndReadBack(
            ProdV3("hello", newField1 = false, Some(200)),
            ProdV5("unset", newField1 = false)
          )
        ),
        test("can read v5 value by skipping the field and using the provided default")(
          canBeSerializedAndReadBack(
            ProdV4("hello", newField1 = false),
            ProdV5("unset", newField1 = false)
          )
        ),
        test("cannot read as v4 because of missing field")(
          cannotBeSerializedAndReadBack[ProdV5, ProdV4](
            ProdV5("hello", newField1 = false),
          )
        ),
      )
    )
}

@nowarn object EvolutionSpec extends EvolutionSpec {
  case class TestId(value: String) extends AnyVal
  object TestId {
    implicit val codec: BinaryCodec[TestId] = BinaryCodec.deriveForWrapper
  }
}