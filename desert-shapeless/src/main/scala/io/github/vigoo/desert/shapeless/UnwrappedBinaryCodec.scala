package io.github.vigoo.desert.shapeless

import io.github.vigoo.desert.BinaryCodec
import io.github.vigoo.desert.BinaryDeserializer.Deser
import io.github.vigoo.desert.BinarySerializer.Ser
import _root_.shapeless._

abstract class UnwrappedBinaryCodec[T] extends BinaryCodec[T]

object UnwrappedBinaryCodec {
  implicit def derive[A, R](implicit
      gen: Lazy[Generic.Aux[A, R :: HNil]],
      codec: BinaryCodec[R]
  ): UnwrappedBinaryCodec[A] =
    new UnwrappedBinaryCodec[A] {
      override def deserialize(): Deser[A]        = codec.map(r => gen.value.from(r :: HNil)).deserialize()
      override def serialize(value: A): Ser[Unit] = codec.contramap((a: A) => gen.value.to(a).head).serialize(value)
    }
}
