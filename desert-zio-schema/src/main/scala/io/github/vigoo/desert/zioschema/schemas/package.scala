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

  val builtInArrayCodecTypeId: TypeId                                                           = TypeId.parse("scala.Array")
  implicit def builtInArrayCodec[A: ClassTag](implicit elemSchema: Schema[A]): Schema[Array[A]] =
    Schema.CaseClass1[A, Array[A]](
      builtInArrayCodecTypeId,
      Schema.Field[Array[A], A]("element", elemSchema, get0 = _ => ???, set0 = (_, _) => ???),
      (_: A) => ???,
      annotations0 = Chunk(implicitly[ClassTag[A]])
    )

  val builtInTryCodecTypeId: TypeId                                               = TypeId.parse("scala.util.Try")
  implicit def builtInTryCodec[A](implicit elemSchema: Schema[A]): Schema[Try[A]] =
    Schema.CaseClass1[A, Try[A]](
      builtInTryCodecTypeId,
      Schema.Field[Try[A], A]("element", elemSchema, get0 = _ => ???, set0 = (_, _) => ???),
      (_: A) => ???
    )
}
