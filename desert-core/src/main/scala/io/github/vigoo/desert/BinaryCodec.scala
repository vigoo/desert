package io.github.vigoo.desert

import io.github.vigoo.desert.custom._
import io.github.vigoo.desert.custom.pure.{Deser, Ser}

import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

trait BinarySerializer[T] { self =>
  def serialize(value: T)(implicit context: SerializationContext): Unit

  def contramap[U](f: U => T): BinarySerializer[U] = new BinarySerializer[U] {
    override def serialize(value: U)(implicit context: SerializationContext): Unit =
      self.serialize(f(value))
  }

  def contramapOrFail[U](f: U => Either[DesertFailure, T]): BinarySerializer[U] =
    new BinarySerializer[U] {
      override def serialize(value: U)(implicit context: SerializationContext): Unit =
        f(value) match {
          case Left(failure) => failSerializerWith(failure)
          case Right(value)  => self.serialize(value)
        }
    }
}

object BinarySerializer {
  def fromPure[T](serializer: T => Ser[T]): BinarySerializer[T] =
    new BinarySerializer[T] {
      override def serialize(value: T)(implicit ctx: SerializationContext): Unit =
        serializer(value).either.provideService(ctx.env).run(ctx.state.toPure) match {
          case (_, Left(failure))      => throw DesertException(failure)
          case (resultState, Right(_)) => ctx.state.resetTo(resultState)
        }
    }
}

trait BinaryDeserializer[T] {
  self =>

  def deserialize()(implicit ctx: DeserializationContext): T

  def map[U](f: T => U): BinaryDeserializer[U] = new BinaryDeserializer[U] {
    override def deserialize()(implicit ctx: DeserializationContext): U =
      f(self.deserialize())
  }

  def mapOrFail[U](f: T => Either[DesertFailure, U]): BinaryDeserializer[U] =
    new BinaryDeserializer[U] {
      override def deserialize()(implicit ctx: DeserializationContext): U =
        f(self.deserialize()) match {
          case Left(failure) => throw DesertException(failure)
          case Right(value)  => value
        }
    }
}

object BinaryDeserializer {
  def fromPure[T](deserializer: Deser[T]): BinaryDeserializer[T] =
    new BinaryDeserializer[T] {
      override def deserialize()(implicit ctx: DeserializationContext): T =
        deserializer.either.provideService(ctx.env).run(ctx.state.toPure) match {
          case (_, Left(failure))          => throw DesertException(failure)
          case (resultState, Right(value)) =>
            ctx.state.resetTo(resultState)
            value
        }
    }
}

trait BinaryCodec[T] extends BinarySerializer[T] with BinaryDeserializer[T]

object BinaryCodec {
  def apply[T: BinaryCodec]: BinaryCodec[T] = implicitly[BinaryCodec[T]]

  implicit def from[T](serializer: BinarySerializer[T], deserializer: BinaryDeserializer[T]): BinaryCodec[T] =
    new BinaryCodec[T] {
      override def deserialize()(implicit ctx: DeserializationContext): T =
        deserializer.deserialize()

      override def serialize(value: T)(implicit context: SerializationContext): Unit =
        serializer.serialize(value)
    }

  def unknown[T](implicit tag: ClassTag[T]): BinaryCodec[T] =
    new BinaryCodec[T] {
      override def serialize(value: T)(implicit context: SerializationContext): Unit =
        writeUnknown(value)

      override def deserialize()(implicit context: DeserializationContext): T = {
        val value = readUnknown()
        Try(value.asInstanceOf[T]) match {
          case Success(upcasted)  => upcasted
          case Failure(exception) =>
            throw DesertException(
              DesertFailure.SerializationUpcastError(value.getClass, tag.runtimeClass, exception)
            )
        }
      }
    }
}
