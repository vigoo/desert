package io.github.vigoo.desert.zioschema

import zio.Chunk
import zio.schema.{Schema, TypeId}

import scala.reflect.ClassTag
import scala.util.Try

/** Implicit placeholder Schema instances for types that cannot be described with zio-schema but have a valid built-in
  * desert BinaryCodec.
  *
  * The codec derivation detects these placeholders and uses the appropriate binary codec instead of failing.
  *
  * The schema instances are not usable for any other purpose.
  */
package object schemas {

  implicit lazy val builtInThrowableCodecPlaceholder: Schema[Throwable] =
    Schema.fail("__builtin_throwable_codec__")

  val builtInArrayCodecTypeId: TypeId                                                 = TypeId.parse("scala.Array")
  implicit def builtInArrayCodec[A](implicit elemSchema: Schema[A]): Schema[Array[A]] =
    Schema.CaseClass1[A, Array[A]](
      builtInArrayCodecTypeId,
      Schema.Field[Array[A], A]("element", elemSchema, get0 = _ => ???, set0 = (_, _) => ???),
      (_: A) => ???,
      annotations0 = Chunk.empty
    )

  val builtInTryCodecTypeId: TypeId                                               = TypeId.parse("scala.util.Try")
  implicit def builtInTryCodec[A](implicit elemSchema: Schema[A]): Schema[Try[A]] =
    Schema.Enum2[scala.util.Failure[A], scala.util.Success[A], scala.util.Try[A]](
      builtInTryCodecTypeId,
      case1 = Schema.Case(
        "Failure",
        Schema.CaseClass1(
          TypeId.parse("scala.util.Failure"),
          field0 = Schema.Field(
            "exception",
            Schema[Throwable],
            get0 = _.exception,
            set0 = (a: scala.util.Failure[A], v: Throwable) => a.copy(exception = v)
          ),
          defaultConstruct0 = (throwable: Throwable) => scala.util.Failure(throwable)
        ),
        _.asInstanceOf[scala.util.Failure[A]],
        x => x,
        _.isInstanceOf[scala.util.Failure[A]]
      ),
      case2 = Schema.Case(
        "Success",
        Schema.CaseClass1(
          TypeId.parse("scala.util.Success"),
          field0 = Schema
            .Field("value", elemSchema, get0 = _.value, set0 = (a: scala.util.Success[A], v: A) => a.copy(value = v)),
          defaultConstruct0 = (value: A) => scala.util.Success(value)
        ),
        _.asInstanceOf[scala.util.Success[A]],
        x => x,
        _.isInstanceOf[scala.util.Success[A]]
      )
    )

  val typeRegistryTypeId: TypeId          = TypeId.parse("io.github.vigoo.desert.TypeRegistry")
  def codecFromTypeRegistry[A]: Schema[A] =
    Schema.CaseClass1[Nothing, A](
      typeRegistryTypeId,
      Schema.Field[A, Nothing](
        "element",
        Schema.fail("__placeholder_for_unknown_codecs__"),
        get0 = _ => ???,
        set0 = (_, _) => ???
      ),
      (_: A) => ???,
      annotations0 = Chunk.empty
    )
}
