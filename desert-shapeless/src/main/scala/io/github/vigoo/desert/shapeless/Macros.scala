package io.github.vigoo.desert.shapeless

import io.github.vigoo.desert.{BinaryCodec, evolutionSteps}

object Macros {
  import scala.reflect.macros.blackbox.Context
  def deriveImpl[T: c.WeakTypeTag](c: Context): c.Expr[BinaryCodec[T]] = {
    import c.universe._

    val evolutionStepsType        = typeOf[evolutionSteps]
    val evolutionSteps: Seq[Tree] = weakTypeOf[T].typeSymbol.annotations
      .collectFirst {
        case annotation if annotation.tree.tpe <:< evolutionStepsType =>
          annotation.tree match {
            case q"new $_(..$annotationArgs)" =>
              annotationArgs
            case _                            =>
              c.error(c.enclosingPosition, "Invalid evaluationSteps annotation")
              Seq.empty
          }
      }
      .getOrElse(Seq.empty[Tree])
    c.Expr[BinaryCodec[T]](
      q"_root_.io.github.vigoo.desert.shapeless.DerivedBinaryCodec.deriveF(..$evolutionSteps) { api => import api._; derive }"
    )
  }

}
