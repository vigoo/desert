package io.github.vigoo.desert.zioschema

import io.github.vigoo.desert.{BinaryCodec, DesertException, TypeRegistry}
import io.github.vigoo.desert.syntax._
import zio.Chunk
import zio.schema.Schema
import zio.stream.ZPipeline

final class Codec private (typeRegistry: TypeRegistry) extends zio.schema.codec.Codec {

  override def encoder[A](schema: Schema[A]): ZPipeline[Any, Nothing, A, Byte] = ???

  override def decoder[A](schema: Schema[A]): ZPipeline[Any, String, Byte, A] = ???

  override def encode[A](schema: Schema[A]): A => Chunk[Byte] =
    (value: A) =>
      serializeToArray(value, typeRegistry)(DerivedBinaryCodec.derive(schema)) match {
        case Left(failure) => throw new DesertException(failure)
        case Right(array)  => Chunk.fromArray(array)
      }

  override def decode[A](schema: Schema[A]): Chunk[Byte] => Either[String, A] =
    (bytes: Chunk[Byte]) => deserializeFromArray(bytes.toArray, typeRegistry)(getCodecFor(schema)).left.map(_.message)

  private def getCodecFor[A](schema: Schema[A]): BinaryCodec[A] =
    DerivedBinaryCodec.derive(schema) // TODO: cache derived binary codecs

}

object Codec {
  val default: Codec = new Codec(TypeRegistry.empty)

  def withTypeRegistry(typeRegistry: TypeRegistry): Codec = new Codec(typeRegistry)
}
