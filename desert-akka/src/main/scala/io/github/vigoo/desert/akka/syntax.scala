package io.github.vigoo.desert.akka

import akka.util.ByteString
import io.github.vigoo.desert.{BinaryDeserializer, BinarySerialization, BinarySerializer, DesertFailure, TypeRegistry}

object syntax {
  def serializeToByteString[T: BinarySerializer](value: T, typeRegistry: TypeRegistry = TypeRegistry.empty): Either[DesertFailure, ByteString] =
    BinarySerialization.serializeToArray[T](value, typeRegistry).map(ByteString.fromArrayUnsafe)

  def serializeUnknownToByteString(value: Any, typeRegistry: TypeRegistry = TypeRegistry.empty): Either[DesertFailure, ByteString] =
    BinarySerialization.serializeUnknownToArray(value, typeRegistry).map(ByteString.fromArrayUnsafe)

  def deserializeFromByteString[T: BinaryDeserializer](input: ByteString, typeRegistry: TypeRegistry = TypeRegistry.empty): Either[DesertFailure, T] =
    BinarySerialization.deserializeFromArray[T](input.toArray, typeRegistry)

  def deserializeUnknownFromByteString(input: ByteString, typeRegistry: TypeRegistry = TypeRegistry.empty): Either[DesertFailure, Any] =
    BinarySerialization.deserializeUnknownFromArray(input.toArray, typeRegistry)
}
