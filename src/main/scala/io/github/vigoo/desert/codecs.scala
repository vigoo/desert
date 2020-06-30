package io.github.vigoo.desert

import java.nio.charset.StandardCharsets

import cats.instances.either._
import cats.syntax.flatMap._
import io.github.vigoo.desert.BinaryDeserializer.Deser
import io.github.vigoo.desert.BinaryDeserializerOps._
import io.github.vigoo.desert.BinarySerializer.Ser
import io.github.vigoo.desert.BinarySerializerOps._
import io.github.vigoo.desert.SerializerState.{StringAlreadyStored, StringId, StringIsNew}

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
      storeString(value).flatMap {
        case StringAlreadyStored(id) =>
          writeVarInt(-id.value, optimizeForPositive = false)
        case StringIsNew(id) =>
          val raw = value.getBytes(charset)
          for {
            _ <- writeVarInt(raw.size, optimizeForPositive = false)
            _ <- writeBytes(raw)
          } yield ()
      }
    }

    override def deserialize(): Deser[String] = {
      for {
        countOrId <- readVarInt(optimizeForPositive = false)
        result <- if (countOrId < 0) {
          val id = StringId(-countOrId)
          getString(id).flatMap {
            case Some(string) => finishDeserializerWith[String](string)
            case None => failDeserializerWith[String](InvalidStringId(id))
          }
        } else {
          for {
            bytes <- readBytes(countOrId)
            string <- Deser.fromEither(Try(new String(bytes, charset))
              .toEither
              .left.map(reason => DeserializationFailure("Failed to decode string", Some(reason))))
            _ <- storeReadString(string)
          } yield string
        }
      } yield result
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
        knownSize <- readVarInt(optimizeForPositive = false)
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
        _ <- writeVarInt(-1, optimizeForPositive = false)
        _ <- value.foldRight(finishSerializer()) {
          case (elem, rest) => write(true) >> write(elem) >> rest
        }
        _ <- write(false)
      } yield ()
    }

    private def serializeWithKnownSize(value: T, size: Int): Ser[Unit] = {
      for {
        _ <- writeVarInt(size, optimizeForPositive = false)
        _ <- value.foldRight(finishSerializer()) {
          case (elem, rest) => write(elem) >> rest
        }
      } yield ()
    }
  }

  implicit def listCodec[A : BinaryCodec]: BinaryCodec[List[A]] = iterableCodec[A, List[A]]
  implicit def vectorCodec[A : BinaryCodec]: BinaryCodec[Vector[A]] = iterableCodec[A, Vector[A]]
  implicit def setCodec[A : BinaryCodec]: BinaryCodec[Set[A]] = iterableCodec[A, Set[A]]

  implicit def tuple1Codec[T1 : BinaryCodec]: BinaryCodec[Tuple1[T1]] = BinaryCodec.deriveF() { api => import api._; derive }
  implicit def tuple2Codec[T1 : BinaryCodec, T2: BinaryCodec]: BinaryCodec[Tuple2[T1, T2]] = BinaryCodec.deriveF() { api => import api._; derive }
  implicit def tuple3Codec[T1 : BinaryCodec, T2: BinaryCodec, T3: BinaryCodec]: BinaryCodec[Tuple3[T1, T2, T3]] = BinaryCodec.deriveF() { api => import api._; derive }
  implicit def tuple4Codec[T1 : BinaryCodec, T2: BinaryCodec, T3: BinaryCodec, T4: BinaryCodec]: BinaryCodec[Tuple4[T1, T2, T3, T4]] = BinaryCodec.deriveF() { api => import api._; derive }
  implicit def tuple5Codec[T1 : BinaryCodec, T2: BinaryCodec, T3: BinaryCodec, T4: BinaryCodec, T5: BinaryCodec]: BinaryCodec[Tuple5[T1, T2, T3, T4, T5]] = BinaryCodec.deriveF() { api => import api._; derive }
  implicit def tuple6Codec[T1 : BinaryCodec, T2: BinaryCodec, T3: BinaryCodec, T4: BinaryCodec, T5: BinaryCodec, T6: BinaryCodec]: BinaryCodec[Tuple6[T1, T2, T3, T4, T5, T6]] = BinaryCodec.deriveF() { api => import api._; derive }
  implicit def tuple7Codec[T1 : BinaryCodec, T2: BinaryCodec, T3: BinaryCodec, T4: BinaryCodec, T5: BinaryCodec, T6: BinaryCodec, T7: BinaryCodec]: BinaryCodec[Tuple7[T1, T2, T3, T4, T5, T6, T7]] = BinaryCodec.deriveF() { api => import api._; derive }
  implicit def tuple8Codec[T1 : BinaryCodec, T2: BinaryCodec, T3: BinaryCodec, T4: BinaryCodec, T5: BinaryCodec, T6: BinaryCodec, T7: BinaryCodec, T8: BinaryCodec]: BinaryCodec[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]] = BinaryCodec.deriveF() { api => import api._; derive }
  implicit def tuple9Codec[T1 : BinaryCodec, T2: BinaryCodec, T3: BinaryCodec, T4: BinaryCodec, T5: BinaryCodec, T6: BinaryCodec, T7: BinaryCodec, T8: BinaryCodec, T9: BinaryCodec]: BinaryCodec[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]] = BinaryCodec.deriveF() { api => import api._; derive }
  implicit def tuple10Codec[T1 : BinaryCodec, T2: BinaryCodec, T3: BinaryCodec, T4: BinaryCodec, T5: BinaryCodec, T6: BinaryCodec, T7: BinaryCodec, T8: BinaryCodec, T9: BinaryCodec, T10: BinaryCodec]: BinaryCodec[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]] = BinaryCodec.deriveF() { api => import api._; derive }
  implicit def tuple11Codec[T1 : BinaryCodec, T2: BinaryCodec, T3: BinaryCodec, T4: BinaryCodec, T5: BinaryCodec, T6: BinaryCodec, T7: BinaryCodec, T8: BinaryCodec, T9: BinaryCodec, T10: BinaryCodec, T11: BinaryCodec]: BinaryCodec[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]] = BinaryCodec.deriveF() { api => import api._; derive }
  implicit def tuple12Codec[T1 : BinaryCodec, T2: BinaryCodec, T3: BinaryCodec, T4: BinaryCodec, T5: BinaryCodec, T6: BinaryCodec, T7: BinaryCodec, T8: BinaryCodec, T9: BinaryCodec, T10: BinaryCodec, T11: BinaryCodec, T12: BinaryCodec]: BinaryCodec[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]] = BinaryCodec.deriveF() { api => import api._; derive }
  implicit def tuple13Codec[T1 : BinaryCodec, T2: BinaryCodec, T3: BinaryCodec, T4: BinaryCodec, T5: BinaryCodec, T6: BinaryCodec, T7: BinaryCodec, T8: BinaryCodec, T9: BinaryCodec, T10: BinaryCodec, T11: BinaryCodec, T12: BinaryCodec, T13: BinaryCodec]: BinaryCodec[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]] = BinaryCodec.deriveF() { api => import api._; derive }
  implicit def tuple14Codec[T1 : BinaryCodec, T2: BinaryCodec, T3: BinaryCodec, T4: BinaryCodec, T5: BinaryCodec, T6: BinaryCodec, T7: BinaryCodec, T8: BinaryCodec, T9: BinaryCodec, T10: BinaryCodec, T11: BinaryCodec, T12: BinaryCodec, T13: BinaryCodec, T14: BinaryCodec]: BinaryCodec[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]] = BinaryCodec.deriveF() { api => import api._; derive }
  implicit def tuple15Codec[T1 : BinaryCodec, T2: BinaryCodec, T3: BinaryCodec, T4: BinaryCodec, T5: BinaryCodec, T6: BinaryCodec, T7: BinaryCodec, T8: BinaryCodec, T9: BinaryCodec, T10: BinaryCodec, T11: BinaryCodec, T12: BinaryCodec, T13: BinaryCodec, T14: BinaryCodec, T15: BinaryCodec]: BinaryCodec[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]] = BinaryCodec.deriveF() { api => import api._; derive }
  implicit def tuple16Codec[T1 : BinaryCodec, T2: BinaryCodec, T3: BinaryCodec, T4: BinaryCodec, T5: BinaryCodec, T6: BinaryCodec, T7: BinaryCodec, T8: BinaryCodec, T9: BinaryCodec, T10: BinaryCodec, T11: BinaryCodec, T12: BinaryCodec, T13: BinaryCodec, T14: BinaryCodec, T15: BinaryCodec, T16: BinaryCodec]: BinaryCodec[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]] = BinaryCodec.deriveF() { api => import api._; derive }
  implicit def tuple17Codec[T1 : BinaryCodec, T2: BinaryCodec, T3: BinaryCodec, T4: BinaryCodec, T5: BinaryCodec, T6: BinaryCodec, T7: BinaryCodec, T8: BinaryCodec, T9: BinaryCodec, T10: BinaryCodec, T11: BinaryCodec, T12: BinaryCodec, T13: BinaryCodec, T14: BinaryCodec, T15: BinaryCodec, T16: BinaryCodec, T17: BinaryCodec]: BinaryCodec[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]] = BinaryCodec.deriveF() { api => import api._; derive }
  implicit def tuple18Codec[T1 : BinaryCodec, T2: BinaryCodec, T3: BinaryCodec, T4: BinaryCodec, T5: BinaryCodec, T6: BinaryCodec, T7: BinaryCodec, T8: BinaryCodec, T9: BinaryCodec, T10: BinaryCodec, T11: BinaryCodec, T12: BinaryCodec, T13: BinaryCodec, T14: BinaryCodec, T15: BinaryCodec, T16: BinaryCodec, T17: BinaryCodec, T18: BinaryCodec]: BinaryCodec[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]] = BinaryCodec.deriveF() { api => import api._; derive }
  implicit def tuple19Codec[T1 : BinaryCodec, T2: BinaryCodec, T3: BinaryCodec, T4: BinaryCodec, T5: BinaryCodec, T6: BinaryCodec, T7: BinaryCodec, T8: BinaryCodec, T9: BinaryCodec, T10: BinaryCodec, T11: BinaryCodec, T12: BinaryCodec, T13: BinaryCodec, T14: BinaryCodec, T15: BinaryCodec, T16: BinaryCodec, T17: BinaryCodec, T18: BinaryCodec, T19: BinaryCodec]: BinaryCodec[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]] = BinaryCodec.deriveF() { api => import api._; derive }
  implicit def tuple20Codec[T1 : BinaryCodec, T2: BinaryCodec, T3: BinaryCodec, T4: BinaryCodec, T5: BinaryCodec, T6: BinaryCodec, T7: BinaryCodec, T8: BinaryCodec, T9: BinaryCodec, T10: BinaryCodec, T11: BinaryCodec, T12: BinaryCodec, T13: BinaryCodec, T14: BinaryCodec, T15: BinaryCodec, T16: BinaryCodec, T17: BinaryCodec, T18: BinaryCodec, T19: BinaryCodec, T20: BinaryCodec]: BinaryCodec[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]] = BinaryCodec.deriveF() { api => import api._; derive }
  implicit def tuple21Codec[T1 : BinaryCodec, T2: BinaryCodec, T3: BinaryCodec, T4: BinaryCodec, T5: BinaryCodec, T6: BinaryCodec, T7: BinaryCodec, T8: BinaryCodec, T9: BinaryCodec, T10: BinaryCodec, T11: BinaryCodec, T12: BinaryCodec, T13: BinaryCodec, T14: BinaryCodec, T15: BinaryCodec, T16: BinaryCodec, T17: BinaryCodec, T18: BinaryCodec, T19: BinaryCodec, T20: BinaryCodec, T21: BinaryCodec]: BinaryCodec[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]] = BinaryCodec.deriveF() { api => import api._; derive }
  implicit def tuple22Codec[T1 : BinaryCodec, T2: BinaryCodec, T3: BinaryCodec, T4: BinaryCodec, T5: BinaryCodec, T6: BinaryCodec, T7: BinaryCodec, T8: BinaryCodec, T9: BinaryCodec, T10: BinaryCodec, T11: BinaryCodec, T12: BinaryCodec, T13: BinaryCodec, T14: BinaryCodec, T15: BinaryCodec, T16: BinaryCodec, T17: BinaryCodec, T18: BinaryCodec, T19: BinaryCodec, T20: BinaryCodec, T21: BinaryCodec, T22: BinaryCodec]: BinaryCodec[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]] = BinaryCodec.deriveF() { api => import api._; derive }

  implicit def mapCodec[K : BinaryCodec, V: BinaryCodec]: BinaryCodec[Map[K, V]] = iterableCodec[(K, V), Map[K, V]]
}