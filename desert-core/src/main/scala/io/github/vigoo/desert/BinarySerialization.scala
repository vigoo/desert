package io.github.vigoo.desert

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

import BinarySerializerOps._
import BinaryDeserializerOps._
import io.github.vigoo.desert.BinaryDeserializer.DeserializationEnv
import io.github.vigoo.desert.BinarySerializer.SerializationEnv
import zio.{BootstrapRuntime, Runtime, ZEnv, ZIO}

trait BinarySerialization {
  this: Runtime[ZEnv] =>

  private def run[T](f: ZIO[ZEnv, DesertFailure, T]): Either[DesertFailure, T] =
    unsafeRun(f.either)

  def serialize[T : BinarySerializer](value: T, output: BinaryOutput, typeRegistry: TypeRegistry = TypeRegistry.empty): Either[DesertFailure, Unit] =
    run(
      SerializationEnv.create(output, typeRegistry) >>> write[T](value)
    )

  def serializeUnknown(value: Any, output: BinaryOutput, typeRegistry: TypeRegistry = TypeRegistry.empty): Either[DesertFailure, Unit] =
    run(
      SerializationEnv.create(output, typeRegistry) >>> writeUnknown(value)
    )

  def deserialize[T: BinaryDeserializer](input: BinaryInput, typeRegistry: TypeRegistry = TypeRegistry.empty): Either[DesertFailure, T] =
    run(
      DeserializationEnv.create(input, typeRegistry) >>> read[T]()
    )

  def deserializeUnknown(input: BinaryInput, typeRegistry: TypeRegistry = TypeRegistry.empty): Either[DesertFailure, Any] =
    run(
      DeserializationEnv.create(input, typeRegistry) >>> readUnknown()
    )

  def serializeToArray[T: BinarySerializer](value: T, typeRegistry: TypeRegistry = TypeRegistry.empty): Either[DesertFailure, Array[Byte]] = {
    val stream = new ByteArrayOutputStream()
    val output = new JavaStreamBinaryOutput(stream)
    serialize(value, output, typeRegistry).map { _ =>
      stream.flush()
      stream.toByteArray
    }
  }

  def serializeUnknownToArray(value: Any, typeRegistry: TypeRegistry = TypeRegistry.empty): Either[DesertFailure, Array[Byte]] = {
    val stream = new ByteArrayOutputStream()
    val output = new JavaStreamBinaryOutput(stream)
    serializeUnknown(value, output, typeRegistry).map { _ =>
      stream.flush()
      stream.toByteArray
    }
  }

  def deserializeFromArray[T: BinaryDeserializer](input: Array[Byte], typeRegistry: TypeRegistry = TypeRegistry.empty): Either[DesertFailure, T] = {
    val stream = new ByteArrayInputStream(input)
    val in = new JavaStreamBinaryInput(stream)
    deserialize[T](in, typeRegistry)
  }

  def deserializeUnknownFromArray(input: Array[Byte], typeRegistry: TypeRegistry = TypeRegistry.empty): Either[DesertFailure, Any] = {
    val stream = new ByteArrayInputStream(input)
    val in = new JavaStreamBinaryInput(stream)
    deserializeUnknown(in, typeRegistry)
  }
}

object BinarySerialization extends BinarySerialization with BootstrapRuntime
