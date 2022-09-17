package io.github.vigoo.desert.ziosupport

import io.github.vigoo.desert.{
  BinaryDeserializer,
  BinaryInput,
  BinaryOutput,
  BinarySerialization,
  BinarySerializer,
  DesertFailure,
  TypeRegistry
}
import zio.{Chunk, ZIO}

trait ZioBinarySerialization {
  def serialize[T: BinarySerializer](
      value: T,
      output: BinaryOutput,
      typeRegistry: TypeRegistry = TypeRegistry.empty
  ): ZIO[Any, DesertFailure, Unit] =
    toEffect(BinarySerialization.serialize[T](value, output, typeRegistry))

  def serializeUnknown(
      value: Any,
      output: BinaryOutput,
      typeRegistry: TypeRegistry = TypeRegistry.empty
  ): ZIO[Any, DesertFailure, Unit] =
    toEffect(BinarySerialization.serializeUnknown(value, output, typeRegistry))

  def deserialize[T: BinaryDeserializer](
      input: BinaryInput,
      typeRegistry: TypeRegistry = TypeRegistry.empty
  ): ZIO[Any, DesertFailure, T] =
    toEffect(BinarySerialization.deserialize[T](input, typeRegistry))

  def deserializeUnknown(
      input: BinaryInput,
      typeRegistry: TypeRegistry = TypeRegistry.empty
  ): ZIO[Any, DesertFailure, Any] =
    toEffect(BinarySerialization.deserializeUnknown(input, typeRegistry))

  def serializeToArray[T: BinarySerializer](
      value: T,
      typeRegistry: TypeRegistry = TypeRegistry.empty
  ): ZIO[Any, DesertFailure, Array[Byte]] =
    toEffect(BinarySerialization.serializeToArray[T](value, typeRegistry))

  def serializeToChunk[T: BinarySerializer](
      value: T,
      typeRegistry: TypeRegistry = TypeRegistry.empty
  ): ZIO[Any, DesertFailure, Chunk[Byte]] =
    toEffect(BinarySerialization.serializeToArray[T](value, typeRegistry)).map(Chunk.fromArray)

  def serializeUnknownToArray(
      value: Any,
      typeRegistry: TypeRegistry = TypeRegistry.empty
  ): ZIO[Any, DesertFailure, Array[Byte]] =
    toEffect(BinarySerialization.serializeUnknownToArray(value, typeRegistry))

  def serializeUnknownToChunk(
      value: Any,
      typeRegistry: TypeRegistry = TypeRegistry.empty
  ): ZIO[Any, DesertFailure, Chunk[Byte]] =
    toEffect(BinarySerialization.serializeUnknownToArray(value, typeRegistry)).map(Chunk.fromArray)

  def deserializeFromArray[T: BinaryDeserializer](
      input: Array[Byte],
      typeRegistry: TypeRegistry = TypeRegistry.empty
  ): ZIO[Any, DesertFailure, T] =
    toEffect(BinarySerialization.deserializeFromArray[T](input, typeRegistry))

  def deserializeFromChunk[T: BinaryDeserializer](
      input: Chunk[Byte],
      typeRegistry: TypeRegistry = TypeRegistry.empty
  ): ZIO[Any, DesertFailure, T] =
    toEffect(BinarySerialization.deserializeFromArray[T](input.toArray, typeRegistry))

  def deserializeUnknownFromArray(
      input: Array[Byte],
      typeRegistry: TypeRegistry = TypeRegistry.empty
  ): ZIO[Any, DesertFailure, Any] =
    toEffect(BinarySerialization.deserializeUnknownFromArray(input, typeRegistry))

  def deserializeUnknownFromChunk(
      input: Chunk[Byte],
      typeRegistry: TypeRegistry = TypeRegistry.empty
  ): ZIO[Any, DesertFailure, Any] =
    toEffect(BinarySerialization.deserializeUnknownFromArray(input.toArray, typeRegistry))

  private def toEffect[T](f: => Either[DesertFailure, T]): ZIO[Any, DesertFailure, T] = ZIO.fromEither(f)
}

object ZioBinarySerialization extends ZioBinarySerialization
