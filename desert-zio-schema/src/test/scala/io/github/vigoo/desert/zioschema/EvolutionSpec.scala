package io.github.vigoo.desert.zioschema

import _root_.zio.schema.{DeriveSchema, Schema}
import io.github.vigoo.desert._
import io.github.vigoo.desert.codecs._
import zio.test.Assertion.{equalTo, hasField, hasSameElements}
import zio.test.magnolia.DeriveGen
import zio.test.{Gen, Sized, Spec, TestEnvironment, ZIOSpecDefault}

object EvolutionSpec extends ZIOSpecDefault with SerializationProperties {

  case class TestId(value: String) extends AnyVal

  case class ProdV1(fieldA: String, fieldB: Int)

  object ProdV1 {
    val gen: Gen[Sized, ProdV1] = DeriveGen[ProdV1]
  }

  @evolutionSteps(FieldAdded("newField1", true))
  case class ProdV2(fieldA: String, newField1: Boolean, fieldB: Int)

  object ProdV2 {
    val gen: Gen[Sized, ProdV2] = DeriveGen[ProdV2]
    val steps                   = Seq(
      FieldAdded("newField1", true)
    )
  }

  @evolutionSteps(FieldAdded("newField1", true), FieldMadeOptional("fieldB"))
  case class ProdV3(fieldA: String, newField1: Boolean, fieldB: Option[Int])

  object ProdV3 {
    val gen: Gen[Sized, ProdV3] = DeriveGen[ProdV3]
    val steps                   = Seq(
      FieldAdded("newField1", true),
      FieldMadeOptional("fieldB")
    )
  }

  @evolutionSteps(FieldAdded("newField1", true), FieldMadeOptional("fieldB"), FieldRemoved("fieldB"))
  case class ProdV4(fieldA: String, newField1: Boolean)

  object ProdV4 {
    val gen: Gen[Sized, ProdV4] = DeriveGen[ProdV4]
    val steps                   = Seq(
      FieldAdded("newField1", true),
      FieldMadeOptional("fieldB"),
      FieldRemoved("fieldB")
    )
  }

  @evolutionSteps(
    FieldAdded("newField1", true),
    FieldMadeOptional("fieldB"),
    FieldRemoved("fieldB"),
    FieldMadeTransient("fieldA")
  )
  case class ProdV5(
      @transientField("unset") fieldA: String,
      newField1: Boolean
  )

  object ProdV5 {
    val gen: Gen[Sized, ProdV5] = DeriveGen[ProdV5]
    val steps                   = Seq(
      FieldAdded("newField1", true),
      FieldMadeOptional("fieldB"),
      FieldRemoved("fieldB"),
      FieldMadeTransient("fieldA")
    )
  }

  sealed trait Coprod1

  case class Case11(a: Int) extends Coprod1

  case class Case21(x: String) extends Coprod1

  sealed trait Coprod2

  case class Case12(a: Int) extends Coprod2

  @transientConstructor
  case class TransientCons() extends Coprod2

  case class Case22(x: String) extends Coprod2

  implicit val typeRegistry: TypeRegistry = TypeRegistry.empty

  implicit val schemaProdV1: Schema[ProdV1]   = DeriveSchema.gen
  implicit val schemaProdV2: Schema[ProdV2]   = DeriveSchema.gen
  implicit val schemaProdV3: Schema[ProdV3]   = DeriveSchema.gen
  implicit val schemaProdV4: Schema[ProdV4]   = DeriveSchema.gen
  implicit val schemaProdV5: Schema[ProdV5]   = DeriveSchema.gen
  implicit val schemaCoprod1: Schema[Coprod1] = DeriveSchema.gen
  implicit val schemaCoprod2: Schema[Coprod2] = DeriveSchema.gen

  implicit val v1codec: BinaryCodec[ProdV1]     = DerivedBinaryCodec.derive
  implicit val v2codec: BinaryCodec[ProdV2]     = DerivedBinaryCodec.derive
  implicit val v3codec: BinaryCodec[ProdV3]     = DerivedBinaryCodec.derive
  implicit val v4codec: BinaryCodec[ProdV4]     = DerivedBinaryCodec.derive
  implicit val v5codec: BinaryCodec[ProdV5]     = DerivedBinaryCodec.derive
  implicit val c1codec: BinaryCodec[Coprod1]    = DerivedBinaryCodec.derive
  implicit val c2codec: BinaryCodec[Coprod2]    = DerivedBinaryCodec.derive
  implicit val testIdCodec: BinaryCodec[TestId] = BinaryCodec.from(
    codecs.stringCodec.contramap(_.value),
    codecs.stringCodec.map(TestId.apply)
  ) // TODO: derivation support

  override def spec: Spec[TestEnvironment, Any] =
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
        )
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
        )
      ),
      suite("adding new field")(
        test("product with added field is serializable")(
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
            ProdV2("hello", newField1 = true, 100)
          )
        )
      ),
      suite("making a field optional")(
        test("product with field made optional is serializable")(
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
            ProdV3("hello", newField1 = true, Some(200))
          )
        ),
        test("new version can read v2")(
          canBeSerializedAndReadBack(
            ProdV2("hello", newField1 = false, 200),
            ProdV3("hello", newField1 = false, Some(200))
          )
        )
      ),
      suite("removing a field")(
        test("product with field removed is serializable")(
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
            ProdV4("hello", newField1 = false)
          )
        ),
        test("cannot read as v2 because of missing field")(
          cannotBeSerializedAndReadBack[ProdV4, ProdV2](
            ProdV4("hello", newField1 = false)
          )
        ),
        test("can read as v3, missing field becomes None")(
          canBeSerializedAndReadBack(
            ProdV4("hello", newField1 = false),
            ProdV3("hello", newField1 = false, fieldB = None)
          )
        )
      ),
      suite("making a field transient")(
        test("product with field made transient is serializable")(
          canBeSerialized(
            ProdV5.gen,
            Some((a: ProdV5) =>
              hasField[ProdV5, Boolean]("newField1", _.newField1, equalTo(a.newField1)) &&
                hasField[ProdV5, String]("fieldA", _.fieldA, equalTo("unset"))
            )
          )
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
            ProdV5("hello", newField1 = false)
          )
        )
      ),
      suite("adding new transient constructors")(
        test("adding a new transient constructor keeps binary compatibility")(
          canBeSerializedAndReadBack[Coprod1, Coprod2](
            Case11(5),
            Case12(5)
          ) &&
            canBeSerializedAndReadBack[Coprod2, Coprod1](
              Case12(5),
              Case11(5)
            ) &&
            canBeSerializedAndReadBack[Coprod1, Coprod2](
              Case21("test"),
              Case22("test")
            ) &&
            canBeSerializedAndReadBack[Coprod2, Coprod1](
              Case22("test"),
              Case21("test")
            )
        )
      )
    )

}
