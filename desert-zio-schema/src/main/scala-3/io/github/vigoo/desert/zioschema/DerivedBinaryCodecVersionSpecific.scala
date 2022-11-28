package io.github.vigoo.desert.zioschema

import scala.quoted._

import io.github.vigoo.desert.BinaryCodec
import zio.schema.{ Derive, Deriver, Schema }

trait DerivedBinaryCodecVersionSpecific {
  lazy val deriver: Deriver[BinaryCodec]

  inline def derive[T](implicit schema: Schema[T]): BinaryCodec[T] = Derive.derive[BinaryCodec, T](DerivedBinaryCodec.deriver)

  inline def deriveForWrapper[T]: BinaryCodec[T] = DerivedBinaryCodecVersionSpecific.deriveForWrapper[T]
}

object DerivedBinaryCodecVersionSpecific {
  inline def deriveForWrapper[T]: BinaryCodec[T] = ${ deriveForWrapperImpl[T] }

  private def deriveForWrapperImpl[T: Type](using ctx: Quotes): Expr[BinaryCodec[T]] = {
    import ctx.reflect._

    val tpe = TypeRepr.of[T]
    val tpeTree = TypeTree.of[T]
    val tpeSym = tpeTree.symbol

    if (tpeSym.flags.is(Flags.Case)) {
      val fields = tpeSym.caseFields
      if (fields.size == 1) {
        val field = fields.head
        val fieldType = tpe.memberType(field)

        fieldType.asType match {
          case'[ft] =>
            Expr.summon[BinaryCodec[ft]] match {
              case Some(summonedInnerCodec) =>
                '{
                    BinaryCodec.from(
                      $summonedInnerCodec.contramap((v: T) => ${Select.unique('v.asTerm, field.name).asExprOf[ft]}),
                      $summonedInnerCodec.map((v: ft) => ${Apply(Select.unique(Ref(tpeSym.companionModule), "apply"), List('v.asTerm)).asExprOf[T]})
                    )
                }
              case None =>
                report.errorAndAbort(s"deriveForWrapper could not find a BinaryCodec instance for the wrapped type ${fieldType}")
          }
        }
      } else {
        report.errorAndAbort("deriveForWrapper only works on case classes with a single wrapped field")
      }
    } else {
      report.errorAndAbort("deriveForWrapper only works on case classes")
    }
  }
}
