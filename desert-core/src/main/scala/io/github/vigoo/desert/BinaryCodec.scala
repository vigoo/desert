package io.github.vigoo.desert

import io.github.vigoo.desert.custom._

import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

trait BinarySerializer[T] { self =>
  def serialize(value: T): Ser[Unit]

  def contramap[U](f: U => T): BinarySerializer[U] = (value: U) => self.serialize(f(value))

  def contramapOrFail[U](f: U => Either[DesertFailure, T]): BinarySerializer[U] = (value: U) =>
    f(value) match {
      case Left(value)  => failSerializerWith(value)
      case Right(value) => self.serialize(value)
    }
}

trait BinaryDeserializer[T] { self =>

  def deserialize(): Deser[T]

  def map[U](f: T => U): BinaryDeserializer[U] = () => self.deserialize().map(f)

  def mapOrFail[U](f: T => Either[DesertFailure, U]): BinaryDeserializer[U] = () =>
    self.deserialize().flatMap(value => Deser.fromEither(f(value)))
}

trait BinaryCodec[T] extends BinarySerializer[T] with BinaryDeserializer[T]

object BinaryCodec {
  def apply[T: BinaryCodec]: BinaryCodec[T] = implicitly[BinaryCodec[T]]

  implicit def from[T](serializer: BinarySerializer[T], deserializer: BinaryDeserializer[T]): BinaryCodec[T] =
    new BinaryCodec[T] {
      override def deserialize(): Deser[T]        = deserializer.deserialize()
      override def serialize(value: T): Ser[Unit] = serializer.serialize(value)
    }

  def define[T](serializeFn: T => Ser[Unit])(deserializeFn: Deser[T]): BinaryCodec[T] = new BinaryCodec[T] {
    override def serialize(value: T): Ser[Unit] = serializeFn(value)
    override def deserialize(): Deser[T]        = deserializeFn
  }

  def unknown[T](implicit tag: ClassTag[T]): BinaryCodec[T] =
    define[T](
      writeUnknown
    )(
      readUnknown().flatMap { value =>
        Try(value.asInstanceOf[T]) match {
          case Success(upcasted)  => finishDeserializerWith(upcasted)
          case Failure(exception) =>
            failDeserializerWith(DesertFailure.SerializationUpcastError(value.getClass, tag.runtimeClass, exception))
        }
      }
    )
}
