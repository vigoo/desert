package io.github.vigoo.desert.zioschema

import io.github.vigoo.desert.{
  BinaryCodec,
  DeserializationFailure,
  DeserializingNonExistingChunk,
  DesertException,
  DesertFailure,
  FailedToReadInput,
  FailedToWriteOutput,
  FieldRemovedInSerializedVersion,
  FieldWithoutDefaultValueIsMissing,
  InputEndedUnexpectedly,
  InvalidConstructorId,
  InvalidConstructorName,
  InvalidRefId,
  InvalidStringId,
  InvalidTypeId,
  NonOptionalFieldSerializedAsNone,
  SerializationFailure,
  SerializationUpcastError,
  SerializingTransientConstructor,
  TypeNotRegistered,
  TypeRegistry,
  UnknownFieldReferenceInEvolutionStep,
  UnknownSerializedEvolutionStep
}
import io.github.vigoo.desert.syntax._
import zio.{Cause, Chunk}
import zio.schema.Schema
import zio.schema.codec.BinaryCodec.{BinaryDecoder, BinaryEncoder}
import zio.schema.codec.{DecodeError, Decoder, Encoder}
import zio.stream.ZPipeline

final class Codec private (typeRegistry: TypeRegistry) extends zio.schema.codec.BinaryCodec {

  override def encoderFor[A](schema: Schema[A]): BinaryEncoder[A] =
    new BinaryEncoder[A] {
      override def encode(value: A): Chunk[Byte] =
        serializeToArray(value, typeRegistry)(DerivedBinaryCodec.derive(schema)) match {
          case Left(failure) => throw new DesertException(failure)
          case Right(array)  => Chunk.fromArray(array)
        }

      override def streamEncoder: ZPipeline[Any, Nothing, A, Byte] = ???
    }

  override def decoderFor[A](schema: Schema[A]): BinaryDecoder[A] =
    new BinaryDecoder[A] {
      override def decode(whole: Chunk[Byte]): Either[DecodeError, A] =
        deserializeFromArray(whole.toArray, typeRegistry)(getCodecFor(schema)).left.map(toDecodeError)

      override def streamDecoder: ZPipeline[Any, DecodeError, Byte, A] = ???
    }

  private def getCodecFor[A](schema: Schema[A]): BinaryCodec[A] =
    DerivedBinaryCodec.derive(schema) // TODO: cache derived binary codecs

  private def toDecodeError(desertFailure: DesertFailure): DecodeError =
    desertFailure match {
      case FailedToReadInput(reason)                               =>
        DecodeError.ReadError(Cause.fail(reason), desertFailure.message)
      case InputEndedUnexpectedly()                                =>
        DecodeError.ReadError(Cause.empty, desertFailure.message)
      case DeserializationFailure(message, cause)                  =>
        DecodeError.ReadError(cause.fold[Cause[Throwable]](Cause.empty)(Cause.fail(_)), desertFailure.message)
      case NonOptionalFieldSerializedAsNone(fieldName)             =>
        DecodeError.ReadError(Cause.empty, desertFailure.message)
      case UnknownFieldReferenceInEvolutionStep(name)              =>
        DecodeError.ReadError(Cause.empty, desertFailure.message)
      case UnknownSerializedEvolutionStep(code)                    =>
        DecodeError.ReadError(Cause.empty, desertFailure.message)
      case InvalidStringId(id)                                     =>
        DecodeError.ReadError(Cause.empty, desertFailure.message)
      case FieldRemovedInSerializedVersion(fieldName)              =>
        DecodeError.ReadError(Cause.empty, desertFailure.message)
      case FieldWithoutDefaultValueIsMissing(fieldName)            =>
        DecodeError.ReadError(Cause.empty, desertFailure.message)
      case TypeNotRegistered(cls)                                  =>
        DecodeError.ReadError(Cause.empty, desertFailure.message)
      case InvalidTypeId(id)                                       =>
        DecodeError.ReadError(Cause.empty, desertFailure.message)
      case InvalidConstructorName(name, typ)                       =>
        DecodeError.ReadError(Cause.empty, desertFailure.message)
      case InvalidConstructorId(id, typ)                           =>
        DecodeError.ReadError(Cause.empty, desertFailure.message)
      case InvalidRefId(id)                                        =>
        DecodeError.ReadError(Cause.empty, desertFailure.message)
      case DeserializingNonExistingChunk(chunk)                    =>
        DecodeError.ReadError(Cause.empty, desertFailure.message)
      case SerializationUpcastError(valueType, targetType, reason) => ???
      case SerializingTransientConstructor(name)                   => ???
      case FailedToWriteOutput(reason)                             => ???
      case SerializationFailure(message, cause)                    => ???
    }
}

object Codec {
  val default: Codec = new Codec(TypeRegistry.empty)

  def withTypeRegistry(typeRegistry: TypeRegistry): Codec = new Codec(typeRegistry)
}
