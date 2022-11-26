package io.github.vigoo.desert

import cats.effect.Sync

package object catseffect {
  def serialize[F[_]: Sync, T: BinarySerializer](
      value: T,
      output: BinaryOutput,
      typeRegistry: TypeRegistry = TypeRegistry.empty
  ): F[Unit] =
    toEffect(io.github.vigoo.desert.serialize[T](value, output, typeRegistry))

  def serializeUnknown[F[_]: Sync](
      value: Any,
      output: BinaryOutput,
      typeRegistry: TypeRegistry = TypeRegistry.empty
  ): F[Unit] =
    toEffect(io.github.vigoo.desert.serializeUnknown(value, output, typeRegistry))

  def deserialize[F[_]: Sync, T: BinaryDeserializer](
      input: BinaryInput,
      typeRegistry: TypeRegistry = TypeRegistry.empty
  ): F[T] =
    toEffect(io.github.vigoo.desert.deserialize[T](input, typeRegistry))

  def deserializeUnknown[F[_]: Sync](input: BinaryInput, typeRegistry: TypeRegistry = TypeRegistry.empty): F[Any] =
    toEffect(io.github.vigoo.desert.deserializeUnknown(input, typeRegistry))

  def serializeToArray[F[_]: Sync, T: BinarySerializer](
      value: T,
      typeRegistry: TypeRegistry = TypeRegistry.empty
  ): F[Array[Byte]] =
    toEffect(io.github.vigoo.desert.serializeToArray[T](value, typeRegistry))

  def serializeUnknownToArray[F[_]: Sync](value: Any, typeRegistry: TypeRegistry = TypeRegistry.empty): F[Array[Byte]] =
    toEffect(io.github.vigoo.desert.serializeUnknownToArray(value, typeRegistry))

  def deserializeFromArray[F[_]: Sync, T: BinaryDeserializer](
      input: Array[Byte],
      typeRegistry: TypeRegistry = TypeRegistry.empty
  ): F[T] =
    toEffect(io.github.vigoo.desert.deserializeFromArray[T](input, typeRegistry))

  def deserializeUnknownFromArray[F[_]: Sync](
      input: Array[Byte],
      typeRegistry: TypeRegistry = TypeRegistry.empty
  ): F[Any] =
    toEffect(io.github.vigoo.desert.deserializeUnknownFromArray(input, typeRegistry))

  private def toEffect[F[_]: Sync, T](f: => Either[DesertFailure, T]): F[T] =
    Sync[F].fromEither(f.left.map(new DesertException(_)))
}
