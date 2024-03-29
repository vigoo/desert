package io.github.vigoo.desert

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream, OutputStream}
import io.github.vigoo.desert.internal.{
  DeserializationEnv,
  JavaStreamBinaryInput,
  JavaStreamBinaryOutput,
  SerializationEnv,
  SerializerState
}
import io.github.vigoo.desert.custom._

trait BinarySerialization {
  def serialize[T: BinarySerializer](
      value: T,
      output: BinaryOutput,
      typeRegistry: TypeRegistry = TypeRegistry.empty
  ): Either[DesertFailure, Unit] = {
    implicit val ctx: SerializationContext =
      custom.SerializationContext(SerializationEnv(output, typeRegistry), SerializerState.create)
    try
      Right(write(value))
    catch {
      case DesertException(e) => Left(e)
    }
  }

  def serializeUnknown(
      value: Any,
      output: BinaryOutput,
      typeRegistry: TypeRegistry = TypeRegistry.empty
  ): Either[DesertFailure, Unit] = {
    implicit val ctx: SerializationContext =
      custom.SerializationContext(SerializationEnv(output, typeRegistry), SerializerState.create)
    try
      Right(writeUnknown(value))
    catch {
      case DesertException(e) => Left(e)
    }
  }

  def deserialize[T: BinaryDeserializer](
      input: BinaryInput,
      typeRegistry: TypeRegistry = TypeRegistry.empty
  ): Either[DesertFailure, T] = {
    implicit val ctx: DeserializationContext =
      custom.DeserializationContext(DeserializationEnv(input, typeRegistry), SerializerState.create)
    try
      Right(read())
    catch {
      case DesertException(e) => Left(e)
    }
  }

  def deserializeUnknown(
      input: BinaryInput,
      typeRegistry: TypeRegistry = TypeRegistry.empty
  ): Either[DesertFailure, Any] = {
    implicit val ctx: DeserializationContext =
      custom.DeserializationContext(DeserializationEnv(input, typeRegistry), SerializerState.create)
    try
      Right(readUnknown())
    catch {
      case DesertException(e) => Left(e)
    }
  }

  def serializeToStream[T: BinarySerializer](
      value: T,
      stream: OutputStream,
      typeRegistry: TypeRegistry = TypeRegistry.empty
  ): Either[DesertFailure, Unit] = {
    val output = new JavaStreamBinaryOutput(stream)
    serialize(value, output, typeRegistry).map { _ =>
      stream.flush()
    }
  }

  def serializeUnknownToStream(
      value: Any,
      stream: OutputStream,
      typeRegistry: TypeRegistry = TypeRegistry.empty
  ): Either[DesertFailure, Unit] = {
    val output = new JavaStreamBinaryOutput(stream)
    serializeUnknown(value, output, typeRegistry).map { _ =>
      stream.flush()
    }
  }

  def deserializeFromStream[T: BinaryDeserializer](
      stream: InputStream,
      typeRegistry: TypeRegistry = TypeRegistry.empty
  ): Either[DesertFailure, T] = {
    val in = new JavaStreamBinaryInput(stream)
    deserialize[T](in, typeRegistry)
  }

  def deserializeUnknownFromStream(
      stream: InputStream,
      typeRegistry: TypeRegistry = TypeRegistry.empty
  ): Either[DesertFailure, Any] = {
    val in = new JavaStreamBinaryInput(stream)
    deserializeUnknown(in, typeRegistry)
  }

  def serializeToArray[T: BinarySerializer](
      value: T,
      typeRegistry: TypeRegistry = TypeRegistry.empty
  ): Either[DesertFailure, Array[Byte]] = {
    val stream = new ByteArrayOutputStream(4096)
    serializeToStream(value, stream, typeRegistry).map { _ =>
      stream.toByteArray
    }
  }

  def serializeUnknownToArray(
      value: Any,
      typeRegistry: TypeRegistry = TypeRegistry.empty
  ): Either[DesertFailure, Array[Byte]] = {
    val stream = new ByteArrayOutputStream(4096)
    serializeUnknownToStream(value, stream, typeRegistry).map { _ =>
      stream.toByteArray
    }
  }

  def deserializeFromArray[T: BinaryDeserializer](
      input: Array[Byte],
      typeRegistry: TypeRegistry = TypeRegistry.empty
  ): Either[DesertFailure, T] = {
    val stream = new ByteArrayInputStream(input)
    deserializeFromStream(stream, typeRegistry)
  }

  def deserializeUnknownFromArray(
      input: Array[Byte],
      typeRegistry: TypeRegistry = TypeRegistry.empty
  ): Either[DesertFailure, Any] = {
    val stream = new ByteArrayInputStream(input)
    deserializeUnknownFromStream(stream, typeRegistry)
  }
}
