package io.github.vigoo.desert

import java.nio.charset.StandardCharsets

import cats.instances.either._
import cats.syntax.flatMap._
import io.github.vigoo.desert.BinaryDeserializer.Deser
import io.github.vigoo.desert.BinaryDeserializerOps._
import io.github.vigoo.desert.BinarySerializer.Ser
import io.github.vigoo.desert.BinarySerializerOps._

import scala.collection.{Factory, mutable}
import scala.util.Try

object codecs {

  implicit val byteCodec: BinaryCodec[Byte] = BinaryCodec.define[Byte](value => writeByte(value))(readByte())
  implicit val shortCodec: BinaryCodec[Short] = BinaryCodec.define[Short](value => writeShort(value))(readShort())
  implicit val intCodec: BinaryCodec[Int] = BinaryCodec.define[Int](value => writeInt(value))(readInt())
  implicit val longCodec: BinaryCodec[Long] = BinaryCodec.define[Long](value => writeLong(value))(readLong())

  implicit val booleanCodec: BinaryCodec[Boolean] = BinaryCodec.define[Boolean](value => writeByte(if (value) 1 else 0))(readByte().map(_ != 0))

  implicit val stringCodec: BinaryCodec[String] = new BinaryCodec[String] {
    private val charset = StandardCharsets.UTF_8

    override def serialize(value: String): Ser[Unit] = {
      val raw = value.getBytes(charset)
      for {
        _ <- writeInt(raw.size)
        _ <- writeBytes(raw)
      } yield ()
    }

    override def deserialize(): Deser[String] = {
      for {
        count <- readInt()
        bytes <- readBytes(count)
        string <- Deser.fromEither(Try(new String(bytes, charset))
          .toEither
          .left.map(reason => DeserializationFailure("Failed to decode string", Some(reason))))
      } yield string
    }
  }

  implicit def optionCodec[T : BinaryCodec]: BinaryCodec[Option[T]] = new BinaryCodec[Option[T]] {
    override def deserialize(): Deser[Option[T]] = {
      for {
        isDefined <- read[Boolean]()
        result <- if (isDefined) read[T]().map(Some.apply) else finishDeserializerWith(None)
      } yield result
    }

    override def serialize(value: Option[T]): Ser[Unit] = {
      value match {
        case Some(value) => write(true) >> write(value)
        case None => write(false)
      }
    }
  }

  def iterableCodec[A : BinaryCodec, T <: Iterable[A]](implicit factory: Factory[A, T]): BinaryCodec[T] = new BinaryCodec[T] {
    override def deserialize(): Deser[T] = {
      for {
        knownSize <- readInt()
        result <- if (knownSize == -1) {
          deserializeWithUnknownSize()
        } else {
          deserializeWithKnownSize(knownSize)
        }
      } yield result
    }

    private def deserializeWithUnknownSize(): Deser[T] = {
      val builder = factory.newBuilder
      readAll(builder).map(_.result())
    }

    private def readAll(builder: mutable.Builder[A, T]): Deser[mutable.Builder[A, T]] =
      readNext().flatMap {
        case None => finishDeserializerWith(builder)
        case Some(elem) => readAll(builder += elem)
      }

    private def readNext(): Deser[Option[A]] = {
      read[Option[A]]()
    }

    private def deserializeWithKnownSize(size: Int): Deser[T] = {
      val builder = factory.newBuilder
      builder.sizeHint(size)

      (0 until size).foldLeft(finishDeserializerWith(builder)) {
        case (b, _) => b.flatMap { b => read[A]().map(b += _) }
      }.map(_.result())
    }

    override def serialize(value: T): Ser[Unit] = {
      if (value.knownSize == -1) {
        serializeWithUnknownSize(value)
      } else {
        serializeWithKnownSize(value, value.knownSize)
      }
    }

    private def serializeWithUnknownSize(value: T): Ser[Unit] = {
      for {
        _ <- writeInt(-1)
        _ <- value.foldRight(finishSerializer()) {
          case (elem, rest) => write(true) >> write(elem) >> rest
        }
        _ <- write(false)
      } yield ()
    }

    private def serializeWithKnownSize(value: T, size: Int): Ser[Unit] = {
      for {
        _ <- writeInt(size)
        _ <- value.foldRight(finishSerializer()) {
          case (elem, rest) => write(elem) >> rest
        }
      } yield ()
    }
  }

  implicit def listCodec[A : BinaryCodec]: BinaryCodec[List[A]] = iterableCodec[A, List[A]]
  implicit def vectorCodec[A : BinaryCodec]: BinaryCodec[Vector[A]] = iterableCodec[A, Vector[A]]
  implicit def setCodec[A : BinaryCodec]: BinaryCodec[Set[A]] = iterableCodec[A, Set[A]]
  // implicit def mapCodec[K : BinaryCodec, V: BinaryCodec]: BinaryCodec[Map[K, V]] = iterableCodec[(K, V), Map[K, V]]
}