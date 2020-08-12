package io.github.vigoo.desert

import java.nio.charset.StandardCharsets
import java.util.UUID

import cats.{Eval, Foldable, Monad, Order, Traverse}
import cats.data.{NonEmptyList, NonEmptyMap, NonEmptySet, Validated}
import cats.instances.either._
import cats.syntax.flatMap._
import io.github.vigoo.desert.BinaryDeserializer.Deser
import io.github.vigoo.desert.BinaryDeserializerOps._
import io.github.vigoo.desert.BinarySerializer.Ser
import io.github.vigoo.desert.BinarySerializerOps._
import io.github.vigoo.desert.SerializerState.{StringAlreadyStored, StringId, StringIsNew}

import scala.collection.{Factory, mutable}
import scala.collection.immutable.{ArraySeq, SortedMap, SortedSet}
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

/**
 * Module containing implicit binary codecs for a lot of base types
 */
object codecs {

  implicit val byteCodec: BinaryCodec[Byte] = BinaryCodec.define[Byte](value => writeByte(value))(readByte())
  implicit val shortCodec: BinaryCodec[Short] = BinaryCodec.define[Short](value => writeShort(value))(readShort())
  implicit val intCodec: BinaryCodec[Int] = BinaryCodec.define[Int](value => writeInt(value))(readInt())
  implicit val longCodec: BinaryCodec[Long] = BinaryCodec.define[Long](value => writeLong(value))(readLong())
  implicit val floatCodec: BinaryCodec[Float] = BinaryCodec.define[Float](value => writeFloat(value))(readFloat())
  implicit val doubleCodec: BinaryCodec[Double] = BinaryCodec.define[Double](value => writeDouble(value))(readDouble())

  implicit val booleanCodec: BinaryCodec[Boolean] = BinaryCodec.define[Boolean](value => writeByte(if (value) 1 else 0))(readByte().map(_ != 0))

  implicit val unitCodec: BinaryCodec[Unit] = BinaryCodec.define[Unit](_ => finishSerializer())(finishDeserializerWith(()))

  implicit val stringCodec: BinaryCodec[String] = new BinaryCodec[String] {
    private val charset = StandardCharsets.UTF_8

    override def serialize(value: String): Ser[Unit] = {
      val raw = value.getBytes(charset)
      for {
        _ <- writeVarInt(raw.size, optimizeForPositive = false)
        _ <- writeBytes(raw)
      } yield ()
    }

    override def deserialize(): Deser[String] = {
      for {
        count <- readVarInt(optimizeForPositive = false)
        bytes <- readBytes(count)
        string <- Deser.fromEither(Try(new String(bytes, charset))
          .toEither
          .left.map(reason => DeserializationFailure("Failed to decode string", Some(reason))))
      } yield string
    }
  }

  case class DeduplicatedString(string: String) extends AnyVal

  implicit val deduplicatedStringCodec: BinaryCodec[DeduplicatedString] = new BinaryCodec[DeduplicatedString] {
    private val charset = StandardCharsets.UTF_8

    override def serialize(value: DeduplicatedString): Ser[Unit] = {
      storeString(value.string).flatMap {
        case StringAlreadyStored(id) =>
          writeVarInt(-id.value, optimizeForPositive = false)
        case StringIsNew(id) =>
          stringCodec.serialize(value.string)
      }
    }

    override def deserialize(): Deser[DeduplicatedString] = {
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
      } yield DeduplicatedString(result)
    }
  }

  implicit val uuidCodec: BinaryCodec[UUID] = BinaryCodec.define(
    (uuid: UUID) => for {
      _ <- writeLong(uuid.getMostSignificantBits)
      _ <- writeLong(uuid.getLeastSignificantBits)
    } yield ()
  )(for {
    msb <- readLong()
    lsb <- readLong()
  } yield new UUID(msb, lsb))

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

  // Throwable

  implicit val stackTraceElementCodec: BinaryCodec[StackTraceElement] = BinaryCodec.define[StackTraceElement](
    stackTraceElement => for {
      _ <- writeByte(0) // version
      _ <- write(Option(stackTraceElement.getClassName))
      _ <- write(Option(stackTraceElement.getMethodName))
      _ <- write(Option(stackTraceElement.getFileName))
      _ <- writeVarInt(stackTraceElement.getLineNumber, optimizeForPositive = true)
    } yield ()
  )(
    for {
      _ <- readByte() // version
      className <- read[Option[String]]()
      methodName <- read[Option[String]]()
      fileName <- read[Option[String]]()
      lineNumber <- readVarInt(optimizeForPositive = true)
    } yield new StackTraceElement(className.orNull, methodName.orNull, fileName.orNull, lineNumber)
  )

  implicit def persistedThrowableCodec: BinaryCodec[PersistedThrowable] = BinaryCodec.deriveF() { api => import api._; derive }
  implicit val throwableCodec: BinaryCodec[Throwable] = BinaryCodec.from(
    persistedThrowableCodec.contramap(PersistedThrowable.apply),
    persistedThrowableCodec.map(persisted => persisted)
  )

  // Collections

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
        _ <- Foldable.iterateRightDefer(value, finishSerializer()) {
          case (elem, rest) => write(true) >> write(elem) >> rest
        }
        _ <- write(false)
      } yield ()
    }

    private def serializeWithKnownSize(value: T, size: Int): Ser[Unit] = {
      for {
        _ <- writeVarInt(size, optimizeForPositive = false)
        _ <- Foldable.iterateRightDefer(value, finishSerializer()) {
          case (elem, rest) => write(elem) >> rest
        }
      } yield ()
    }
  }

  implicit def wrappedArrayCodec[A: BinaryCodec : ClassTag]: BinaryCodec[ArraySeq[A]] = iterableCodec[A, ArraySeq[A]]
  implicit def arrayCodec[A: BinaryCodec : ClassTag]: BinaryCodec[Array[A]] = BinaryCodec.from(
    wrappedArrayCodec.contramap(arr => ArraySeq.unsafeWrapArray(arr)),
    wrappedArrayCodec.map(_.toArray[A])
  )
  implicit def listCodec[A : BinaryCodec]: BinaryCodec[List[A]] = iterableCodec[A, List[A]]
  implicit def vectorCodec[A : BinaryCodec]: BinaryCodec[Vector[A]] = iterableCodec[A, Vector[A]]
  implicit def setCodec[A : BinaryCodec]: BinaryCodec[Set[A]] = iterableCodec[A, Set[A]]
  implicit def sortedSetCodec[A : BinaryCodec : Ordering]: BinaryCodec[SortedSet[A]] = iterableCodec[A, SortedSet[A]]

  // Tuples

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
  implicit def sortedMapCodec[K : BinaryCodec : Ordering, V: BinaryCodec]: BinaryCodec[SortedMap[K, V]] = iterableCodec[(K, V), SortedMap[K, V]]

  implicit def eitherCodec[L: BinaryCodec, R: BinaryCodec]: BinaryCodec[Either[L, R]] = new BinaryCodec[Either[L, R]] {
    override def deserialize(): Deser[Either[L, R]] = {
      for {
        isRight <- read[Boolean]()
        result <- if (isRight) read[R]().map(Right.apply) else read[L]().map(Left.apply)
      } yield result
    }

    override def serialize(value: Either[L, R]): Ser[Unit] = {
      value match {
        case Left(value) => write(false) >> write(value)
        case Right(value) => write(true) >> write(value)
      }
    }
  }

  implicit def tryCodec[A: BinaryCodec]: BinaryCodec[Try[A]] = new BinaryCodec[Try[A]] {
    override def deserialize(): Deser[Try[A]] = {
      for {
        isSuccess <- read[Boolean]()
        result <- if (isSuccess) read[A]().map(Success.apply) else read[Throwable]().map(Failure.apply)
      } yield result
    }

    override def serialize(value: Try[A]): Ser[Unit] = {
      value match {
        case Failure(reason) => write(false) >> write(reason)
        case Success(value) => write(true) >> write(value)
      }
    }
  }

  // Cats specific codecs

  implicit def validatedCodec[E: BinaryCodec, A: BinaryCodec]: BinaryCodec[Validated[E, A]] = new BinaryCodec[Validated[E, A]] {
    override def deserialize(): Deser[Validated[E, A]] = {
      for {
        isValid <- read[Boolean]()
        result <- if (isValid) read[A]().map(Validated.Valid.apply) else read[E]().map(Validated.Invalid.apply)
      } yield result
    }

    override def serialize(value: Validated[E, A]): Ser[Unit] = {
      value match {
        case Validated.Invalid(value) => write(false) >> write(value)
        case Validated.Valid(value) => write(true) >> write(value)
      }
    }
  }

  implicit def nonEmptyListCodec[A: BinaryCodec]: BinaryCodec[NonEmptyList[A]] = {
    val inner = listCodec[A]
    BinaryCodec.from(
      inner.contramap(_.toList),
      () => inner.deserialize().flatMap { list =>
        NonEmptyList.fromList(list) match {
          case Some(value) => finishDeserializerWith(value)
          case None => failDeserializerWith(DeserializationFailure("Non empty list is serialized as empty", None))
        }
      }
    )
  }

  implicit def nonEmptySetCodec[A: BinaryCodec : Ordering]: BinaryCodec[NonEmptySet[A]] = {
    val inner = sortedSetCodec[A]
    BinaryCodec.from(
      inner.contramap(_.toSortedSet),
      () => inner.deserialize().flatMap { set =>
        NonEmptySet.fromSet(set) match {
          case Some(value) => finishDeserializerWith(value)
          case None => failDeserializerWith(DeserializationFailure("Non empty set is serialized as empty", None))
        }
      }
    )
  }

  implicit def nonEmptyMapCodec[K: BinaryCodec : Ordering : Order, V: BinaryCodec]: BinaryCodec[NonEmptyMap[K, V]] = {
    val innner = sortedMapCodec[K, V]
    BinaryCodec.from(
      innner.contramap(_.toSortedMap),
      () => innner.deserialize().flatMap { map =>
        NonEmptyMap.fromMap(map) match {
          case Some(value) => finishDeserializerWith(value)
          case None => failDeserializerWith(DeserializationFailure("Non empty map is serialized as empty", None))
        }
      }
    )
  }
}