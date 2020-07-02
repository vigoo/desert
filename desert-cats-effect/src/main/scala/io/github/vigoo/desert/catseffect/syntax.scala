package io.github.vigoo.desert.catseffect

import cats.effect._
import io.github.vigoo.desert._

object syntax {
  def serialize[F[_] : Sync, T: BinarySerializer](value: T, output: BinaryOutput, typeRegistry: TypeRegistry = TypeRegistry.empty): F[Unit] =
    toEffect(BinarySerialization.serialize[T](value, output, typeRegistry))

  def serializeUnknown[F[_] : Sync](value: Any, output: BinaryOutput, typeRegistry: TypeRegistry = TypeRegistry.empty): F[Unit] =
    toEffect(BinarySerialization.serializeUnknown(value, output, typeRegistry))

  def deserialize[F[_] : Sync, T: BinaryDeserializer](input: BinaryInput, typeRegistry: TypeRegistry = TypeRegistry.empty): F[T] =
    toEffect(BinarySerialization.deserialize[T](input, typeRegistry))

  def deserializeUnknown[F[_] : Sync](input: BinaryInput, typeRegistry: TypeRegistry = TypeRegistry.empty): F[Any] =
    toEffect(BinarySerialization.deserializeUnknown(input, typeRegistry))

  def serializeToArray[F[_] : Sync, T: BinarySerializer](value: T, typeRegistry: TypeRegistry = TypeRegistry.empty): F[Array[Byte]] =
    toEffect(BinarySerialization.serializeToArray[T](value, typeRegistry))

  def serializeUnknownToArray[F[_] : Sync](value: Any, typeRegistry: TypeRegistry = TypeRegistry.empty): F[Array[Byte]] =
    toEffect(BinarySerialization.serializeUnknownToArray(value, typeRegistry))

  def deserializeFromArray[F[_] : Sync, T: BinaryDeserializer](input: Array[Byte], typeRegistry: TypeRegistry = TypeRegistry.empty): F[T] =
    toEffect(BinarySerialization.deserializeFromArray[T](input, typeRegistry))

  def deserializeUnknownFromArray[F[_] : Sync](input: Array[Byte], typeRegistry: TypeRegistry = TypeRegistry.empty): F[Any] =
    toEffect(BinarySerialization.deserializeUnknownFromArray(input, typeRegistry))

  private def toEffect[F[_] : Sync, T](f: => Either[DesertFailure, T]): F[T] =
    Sync[F].fromEither(f.left.map(new DesertException(_)))
}
