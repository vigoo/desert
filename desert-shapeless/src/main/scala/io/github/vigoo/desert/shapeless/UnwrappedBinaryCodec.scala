package io.github.vigoo.desert.shapeless

import io.github.vigoo.desert._
import _root_.shapeless._
import io.github.vigoo.desert.internal.{DeserializationContext, SerializationContext}

abstract class UnwrappedBinaryCodec[T] extends BinaryCodec[T]

object UnwrappedBinaryCodec {
  implicit def derive[A, R](implicit
      gen: Lazy[Generic.Aux[A, R :: HNil]],
      codec: BinaryCodec[R]
  ): UnwrappedBinaryCodec[A] =
    new UnwrappedBinaryCodec[A] {
      override def deserialize()(implicit ctx: DeserializationContext): A =
        gen.value.from(codec.deserialize() :: HNil)

      override def serialize(value: A)(implicit context: SerializationContext): Unit =
        codec.serialize(gen.value.to(value).head)
    }
}
