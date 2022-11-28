package io.github.vigoo.desert

import _root_.akka.util.ByteString

package object akkasupport extends Codecs {
  def serializeToByteString[T: BinarySerializer](
      value: T,
      typeRegistry: TypeRegistry = TypeRegistry.empty
  ): Either[DesertFailure, ByteString] =
    io.github.vigoo.desert.serializeToArray[T](value, typeRegistry).map(ByteString.fromArrayUnsafe)

  def serializeUnknownToByteString(
      value: Any,
      typeRegistry: TypeRegistry = TypeRegistry.empty
  ): Either[DesertFailure, ByteString] =
    io.github.vigoo.desert.serializeUnknownToArray(value, typeRegistry).map(ByteString.fromArrayUnsafe)

  def deserializeFromByteString[T: BinaryDeserializer](
      input: ByteString,
      typeRegistry: TypeRegistry = TypeRegistry.empty
  ): Either[DesertFailure, T] =
    io.github.vigoo.desert.deserializeFromArray[T](input.toArray, typeRegistry)

  def deserializeUnknownFromByteString(
      input: ByteString,
      typeRegistry: TypeRegistry = TypeRegistry.empty
  ): Either[DesertFailure, Any] =
    io.github.vigoo.desert.deserializeUnknownFromArray(input.toArray, typeRegistry)
}
