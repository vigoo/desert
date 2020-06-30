package io.github.vigoo.desert

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

import cats.instances.either._
import BinarySerializerOps._
import BinaryDeserializerOps._
import io.github.vigoo.desert.BinaryDeserializer.DeserializationEnv
import io.github.vigoo.desert.BinarySerializer.SerializationEnv

trait BinarySerialization {
  def serialize[T : BinarySerializer](value: T, output: BinaryOutput, typeRegistry: TypeRegistry = TypeRegistry.empty): Either[DesertFailure, Unit] =
    write[T](value).run(SerializationEnv(output, typeRegistry)).runA(SerializerState.initial)

  def serializeUnknown(value: Any, output: BinaryOutput, typeRegistry: TypeRegistry = TypeRegistry.empty): Either[DesertFailure, Unit] =
    writeUnknown(value).run(SerializationEnv(output, typeRegistry)).runA(SerializerState.initial)

  def deserialize[T: BinaryDeserializer](input: BinaryInput, typeRegistry: TypeRegistry = TypeRegistry.empty): Either[DesertFailure, T] =
    read[T]().run(DeserializationEnv(input, typeRegistry)).runA(SerializerState.initial)

  def deserializeUnknown(input: BinaryInput, typeRegistry: TypeRegistry = TypeRegistry.empty): Either[DesertFailure, Any] =
    readUnknown().run(DeserializationEnv(input, typeRegistry)).runA(SerializerState.initial)

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

object BinarySerialization extends BinarySerialization
