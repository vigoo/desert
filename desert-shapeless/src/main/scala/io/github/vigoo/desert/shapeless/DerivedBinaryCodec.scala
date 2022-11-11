package io.github.vigoo.desert.shapeless

import io.github.vigoo.desert._
import _root_.shapeless.Lazy

import scala.language.experimental.macros

object DerivedBinaryCodec {

  def derive[T](evolutionSteps: Evolution*): BinaryCodec[T] = macro Macros.deriveImpl[T]

  def deriveForWrapper[T](implicit codec: Lazy[UnwrappedBinaryCodec[T]]): BinaryCodec[T] = codec.value

  def deriveF[T](evolutionSteps: Evolution*)(f: GenericDerivationApi => BinaryCodec[T]): BinaryCodec[T] =
    if (evolutionSteps.isEmpty) {
      f(GenericBinaryCodec.simple)
    } else {
      f(new GenericBinaryCodec(InitialVersion +: evolutionSteps.toVector))
    }
}
