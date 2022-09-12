package io.github.vigoo.desert.shapeless

import io.github.vigoo.desert.{BinaryCodec, Evolution}

object Macros {
  import scala.reflect.macros.blackbox.Context
  def deriveImpl[T](c: Context)(evolutionSteps: c.Expr[Evolution]*): c.Expr[BinaryCodec[T]] = {
    import c.universe._
    c.Expr(
      q"io.github.vigoo.desert.shapeless.DerivedBinaryCodec.deriveF(..$evolutionSteps) { api => import api._; derive }"
    )
  }

}
