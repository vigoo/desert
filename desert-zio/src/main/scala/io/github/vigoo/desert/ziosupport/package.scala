package io.github.vigoo.desert

import zio._

package object ziosupport extends Codecs {
  def serialize[T: BinarySerializer](
      value: T,
      output: BinaryOutput,
      typeRegistry: TypeRegistry = TypeRegistry.empty
  ): ZIO[Any, DesertFailure, Unit] =
    toEffect(io.github.vigoo.desert.serialize[T](value, output, typeRegistry))

  def serializeUnknown(
      value: Any,
      output: BinaryOutput,
      typeRegistry: TypeRegistry = TypeRegistry.empty
  ): ZIO[Any, DesertFailure, Unit] =
    toEffect(io.github.vigoo.desert.serializeUnknown(value, output, typeRegistry))

  def deserialize[T: BinaryDeserializer](
      input: BinaryInput,
      typeRegistry: TypeRegistry = TypeRegistry.empty
  ): ZIO[Any, DesertFailure, T] =
    toEffect(io.github.vigoo.desert.deserialize[T](input, typeRegistry))

  def deserializeUnknown(
      input: BinaryInput,
      typeRegistry: TypeRegistry = TypeRegistry.empty
  ): ZIO[Any, DesertFailure, Any] =
    toEffect(io.github.vigoo.desert.deserializeUnknown(input, typeRegistry))

  def serializeToArray[T: BinarySerializer](
      value: T,
      typeRegistry: TypeRegistry = TypeRegistry.empty
  ): ZIO[Any, DesertFailure, Array[Byte]] =
    toEffect(io.github.vigoo.desert.serializeToArray[T](value, typeRegistry))

  def serializeToChunk[T: BinarySerializer](
      value: T,
      typeRegistry: TypeRegistry = TypeRegistry.empty
  ): ZIO[Any, DesertFailure, Chunk[Byte]] =
    toEffect(io.github.vigoo.desert.serializeToArray[T](value, typeRegistry)).map(Chunk.fromArray)

  def serializeUnknownToArray(
      value: Any,
      typeRegistry: TypeRegistry = TypeRegistry.empty
  ): ZIO[Any, DesertFailure, Array[Byte]] =
    toEffect(io.github.vigoo.desert.serializeUnknownToArray(value, typeRegistry))

  def serializeUnknownToChunk(
      value: Any,
      typeRegistry: TypeRegistry = TypeRegistry.empty
  ): ZIO[Any, DesertFailure, Chunk[Byte]] =
    toEffect(io.github.vigoo.desert.serializeUnknownToArray(value, typeRegistry)).map(Chunk.fromArray)

  def deserializeFromArray[T: BinaryDeserializer](
      input: Array[Byte],
      typeRegistry: TypeRegistry = TypeRegistry.empty
  ): ZIO[Any, DesertFailure, T] =
    toEffect(io.github.vigoo.desert.deserializeFromArray[T](input, typeRegistry))

  def deserializeFromChunk[T: BinaryDeserializer](
      input: Chunk[Byte],
      typeRegistry: TypeRegistry = TypeRegistry.empty
  ): ZIO[Any, DesertFailure, T] =
    toEffect(io.github.vigoo.desert.deserializeFromArray[T](input.toArray, typeRegistry))

  def deserializeUnknownFromArray(
      input: Array[Byte],
      typeRegistry: TypeRegistry = TypeRegistry.empty
  ): ZIO[Any, DesertFailure, Any] =
    toEffect(io.github.vigoo.desert.deserializeUnknownFromArray(input, typeRegistry))

  def deserializeUnknownFromChunk(
      input: Chunk[Byte],
      typeRegistry: TypeRegistry = TypeRegistry.empty
  ): ZIO[Any, DesertFailure, Any] =
    toEffect(io.github.vigoo.desert.deserializeUnknownFromArray(input.toArray, typeRegistry))

  private def toEffect[T](f: => Either[DesertFailure, T]): ZIO[Any, DesertFailure, T] = ZIO.fromEither(f)
}
