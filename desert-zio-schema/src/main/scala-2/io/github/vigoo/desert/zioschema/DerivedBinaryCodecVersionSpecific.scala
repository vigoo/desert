package io.github.vigoo.desert.zioschema

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

import io.github.vigoo.desert.BinaryCodec
import zio.schema.{Deriver, Schema}

trait DerivedBinaryCodecVersionSpecific {
  def deriver: Deriver[BinaryCodec]

  def derive[T](implicit schema: Schema[T]): BinaryCodec[T] = macro DerivedBinaryCodecVersionSpecific.deriveImpl[T]

  def deriveForWrapper[T]: BinaryCodec[T] = macro DerivedBinaryCodecVersionSpecific.deriveForWrapperImpl[T]
}

object DerivedBinaryCodecVersionSpecific {

  def deriveImpl[T: c.WeakTypeTag](c: whitebox.Context)(schema: c.Expr[Schema[T]]): c.Tree = {
    import c.universe._
    val tpe = weakTypeOf[T]
    q"_root_.zio.schema.Derive.derive[BinaryCodec, $tpe](_root_.io.github.vigoo.desert.zioschema.DerivedBinaryCodec.deriver)($schema)"
  }

  def deriveForWrapperImpl[T: c.WeakTypeTag](c: whitebox.Context): c.Tree = {
    import c.universe._

    val tpe = weakTypeOf[T]
    if (tpe.typeSymbol.asClass.isCaseClass) {
      val fields = tpe.decls.sorted.collect {
        case p: TermSymbol if p.isCaseAccessor && !p.isMethod => p
      }
      if (fields.size == 1) {
        val field              = fields.head
        val fieldType          = field.typeSignature.asSeenFrom(tpe, tpe.typeSymbol.asClass)
        val binaryCodec        = typeOf[BinaryCodec[_]]
        val fieldTypeCodec     = appliedType(binaryCodec, fieldType)
        val summonedInnerCodec = c.inferImplicitValue(fieldTypeCodec)
        if (summonedInnerCodec != EmptyTree) {
          q"""{
           lazy val innerCodec = $summonedInnerCodec
           _root_.io.github.vigoo.desert.BinaryCodec.from(
              innerCodec.contramap((v: $tpe) => v.${TermName(field.name.toString.trim)}),
              innerCodec.map((v: $fieldType) => ${tpe.typeSymbol.companion}.apply(v))
           )
         }"""
        } else {
          c.abort(
            c.enclosingPosition,
            s"deriveForWrapper could not find a BinaryCodec instance for the wrapped type ${fieldType}"
          )
        }
      } else {
        c.abort(c.enclosingPosition, "deriveForWrapper only works on case classes with a single wrapped field")
      }
    } else {
      c.abort(c.enclosingPosition, "deriveForWrapper only works on case classes")
    }
  }
}
