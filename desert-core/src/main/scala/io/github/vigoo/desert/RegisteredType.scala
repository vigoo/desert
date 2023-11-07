package io.github.vigoo.desert

import io.github.vigoo.desert.TypeRegistry.RegisteredTypeId
import io.github.vigoo.desert.custom.{SerializationContext, failSerializerWith}
import io.github.vigoo.desert.internal.{SerializationEnv, SerializerState}

import scala.util.{Failure, Success, Try}

final case class RegisteredType[T](id: RegisteredTypeId, codec: BinaryCodec[T], cls: Class[_]) {
  def serialize(value: Any)(implicit ctx: SerializationContext): Unit =
    Try(value.asInstanceOf[T]) match {
      case Success(upcasted)  => codec.serialize(upcasted)
      case Failure(exception) =>
        failSerializerWith(DesertFailure.SerializationUpcastError(value.getClass, cls, exception))
    }
}
