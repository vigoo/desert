package io.github.vigoo.desert

object Macros {
  import scala.reflect.macros.blackbox.Context
  def deriveImpl[T](c: Context)(evolutionSteps: c.Expr[Evolution]*): c.Expr[BinaryCodec[T]] = {
    import c.universe._
    c.Expr(q"io.github.vigoo.desert.BinaryCodec.deriveF(..$evolutionSteps) { api => import api._; derive }")
  }

}
