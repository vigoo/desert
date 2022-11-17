package io.github.vigoo.desert.zioschema

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

import io.github.vigoo.desert.BinaryCodec
import zio.schema.{ Deriver, Schema }

trait BinaryCodecVersionSpecific {
  val deriver: Deriver[BinaryCodec]

  def derive[T](implicit schema: Schema[T]): BinaryCodec[T] = macro BinaryCodecVersionSpecific.deriveImpl[T]
}

object BinaryCodecVersionSpecific {

  def deriveImpl[T: c.WeakTypeTag](c: whitebox.Context)(schema: c.Expr[Schema[T]]): c.Tree = {
    import c.universe._
    val tpe = weakTypeOf[T]
    q"_root_.zio.schema.Derive.derive[BinaryCodec, $tpe](_root_.io.github.vigoo.desert.zioschema.DerivedBinaryCodec.deriver)($schema)"
  }
}
