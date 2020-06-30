package io.github.vigoo.desert

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

import cats.instances.either._
import BinarySerializerOps._
import BinaryDeserializerOps._

trait BinarySerialization {
  def serialize[T : BinarySerializer](value: T, output: BinaryOutput): Either[DesertFailure, Unit] =
    write[T](value).run(output).runA(SerializerState.initial)

  def deserialize[T: BinaryDeserializer](input: BinaryInput): Either[DesertFailure, T] =
    read[T]().run(input).runA(SerializerState.initial)

  def serializeToArray[T: BinarySerializer](value: T): Either[DesertFailure, Array[Byte]] = {
    val stream = new ByteArrayOutputStream()
    val output = new JavaStreamBinaryOutput(stream)
    serialize(value, output).map { _ =>
      stream.flush()
      stream.toByteArray
    }
  }
  def deserializeFromArray[T: BinaryDeserializer](input: Array[Byte]): Either[DesertFailure, T] = {
    val stream = new ByteArrayInputStream(input)
    val in = new JavaStreamBinaryInput(stream)
    deserialize[T](in)
  }
}

object BinarySerialization extends BinarySerialization
