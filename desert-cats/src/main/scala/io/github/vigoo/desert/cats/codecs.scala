package io.github.vigoo.desert.cats

import cats.Order
import cats.data.{NonEmptyList, NonEmptyMap, NonEmptySet, Validated}
import cats.instances.either._
import cats.syntax.flatMap._

import io.github.vigoo.desert.{BinaryCodec, DeserializationFailure}
import io.github.vigoo.desert.BinaryDeserializer.Deser
import io.github.vigoo.desert.BinaryDeserializerOps.{failDeserializerWith, finishDeserializerWith, read}
import io.github.vigoo.desert.BinarySerializer.Ser
import io.github.vigoo.desert.BinarySerializerOps.write
import io.github.vigoo.desert.codecs._

object codecs {
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
        case Validated.Invalid(value) => write(false) *> write(value)
        case Validated.Valid(value) => write(true) *> write(value)
      }
    }
  }

  implicit def catsNonEmptyListCodec[A: BinaryCodec]: BinaryCodec[NonEmptyList[A]] = {
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

  implicit def catsNonEmptySetCodec[A: BinaryCodec : Ordering]: BinaryCodec[NonEmptySet[A]] = {
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

  implicit def catsNonEmptyMapCodec[K: BinaryCodec : Ordering : Order, V: BinaryCodec]: BinaryCodec[NonEmptyMap[K, V]] = {
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
