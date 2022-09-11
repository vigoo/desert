package io.github.vigoo.desert

import io.github.vigoo.desert.BinarySerializer.Ser
import io.github.vigoo.desert.TypeRegistry.RegisteredTypeId

import scala.util.{Failure, Success, Try}

case class RegisteredType[T](id: RegisteredTypeId, codec: BinaryCodec[T], cls: Class[_]) {
  def serialize(value: Any): Ser[Unit] =
    Try(value.asInstanceOf[T]) match {
      case Success(upcasted)  => codec.serialize(upcasted)
      case Failure(exception) =>
        BinarySerializerOps.failSerializerWith(SerializationUpcastError(value.getClass, cls, exception))
    }
}
