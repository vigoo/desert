package io.github.vigoo.desert.internal

import io.github.vigoo.desert._
import io.github.vigoo.desert.custom.{read, write}

private[desert] final case class OptionBinaryCodec[T]()(implicit val innerCodec: BinaryCodec[T])
    extends BinaryCodec[Option[T]] {

  override def deserialize()(implicit ctx: DeserializationContext): Option[T] =
    if (read[Boolean]()) Some(read[T]()) else None

  override def serialize(value: Option[T])(implicit context: SerializationContext): Unit =
    value match {
      case Some(value) =>
        write(true)
        write(value)
      case None        =>
        write(false)
    }
}
