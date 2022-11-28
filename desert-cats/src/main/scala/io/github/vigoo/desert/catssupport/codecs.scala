package io.github.vigoo.desert

import cats.Order
import cats.data.{NonEmptyList, NonEmptyMap, NonEmptySet, Validated}
import io.github.vigoo.desert.custom._
import io.github.vigoo.desert.internal.{DeserializationContext, SerializationContext}

package object catssupport {
  // Cats specific codecs

  implicit def validatedCodec[E: BinaryCodec, A: BinaryCodec]: BinaryCodec[Validated[E, A]] =
    new BinaryCodec[Validated[E, A]] {
      override def deserialize()(implicit ctx: DeserializationContext): Validated[E, A] = {
        val isValid = read[Boolean]()
        if (isValid) Validated.Valid(read[A]()) else Validated.Invalid(read[E]())
      }

      override def serialize(value: Validated[E, A])(implicit context: SerializationContext): Unit =
        value match {
          case Validated.Valid(a)   => write(true); write(a)
          case Validated.Invalid(e) => write(false); write(e)
        }
    }

  implicit def catsNonEmptyListCodec[A: BinaryCodec]: BinaryCodec[NonEmptyList[A]] = {
    val inner = listCodec[A]
    BinaryCodec.from(
      inner.contramap(_.toList),
      new BinaryDeserializer[NonEmptyList[A]] {
        override def deserialize()(implicit ctx: DeserializationContext): NonEmptyList[A] = {
          val list = inner.deserialize()
          NonEmptyList.fromList(list) match {
            case Some(value) => value
            case None        =>
              failDeserializerWith(DesertFailure.DeserializationFailure("Non empty list is serialized as empty", None))
          }
        }
      }
    )
  }

  implicit def catsNonEmptySetCodec[A: BinaryCodec: Ordering]: BinaryCodec[NonEmptySet[A]] = {
    val inner = sortedSetCodec[A]
    BinaryCodec.from(
      inner.contramap(_.toSortedSet),
      new BinaryDeserializer[NonEmptySet[A]] {
        override def deserialize()(implicit ctx: DeserializationContext): NonEmptySet[A] = {
          val set = inner.deserialize()
          NonEmptySet.fromSet(set) match {
            case Some(value) => value
            case None        =>
              failDeserializerWith(DesertFailure.DeserializationFailure("Non empty set is serialized as empty", None))
          }
        }
      }
    )
  }

  implicit def catsNonEmptyMapCodec[K: BinaryCodec: Ordering: Order, V: BinaryCodec]: BinaryCodec[NonEmptyMap[K, V]] = {
    val innner = sortedMapCodec[K, V]
    BinaryCodec.from(
      innner.contramap(_.toSortedMap),
      new BinaryDeserializer[NonEmptyMap[K, V]] {
        override def deserialize()(implicit ctx: DeserializationContext): NonEmptyMap[K, V] = {
          val map = innner.deserialize()
          NonEmptyMap.fromMap(map) match {
            case Some(value) => value
            case None        =>
              failDeserializerWith(DesertFailure.DeserializationFailure("Non empty map is serialized as empty", None))
          }
        }
      }
    )
  }
}
