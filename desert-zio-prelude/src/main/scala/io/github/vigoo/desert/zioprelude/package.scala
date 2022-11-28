package io.github.vigoo.desert

import io.github.vigoo.desert.custom._
import io.github.vigoo.desert.custom.pure.{Ser, Deser}
import zio.prelude.{NonEmptyList, Validation, ZSet}
import zio.{Chunk, NonEmptyChunk}

package object zioprelude {

  implicit class BinarySerializerZioPreludeOps(val obj: BinarySerializer.type) extends AnyVal {
    def fromPure[T](serializer: T => Ser[T]): BinarySerializer[T] =
      new BinarySerializer[T] {
        override def serialize(value: T)(implicit ctx: SerializationContext): Unit =
          serializer(value).either.provideService(ctx.env).run(ctx.state.toPure) match {
            case (_, Left(failure))      => throw DesertException(failure)
            case (resultState, Right(_)) => ctx.state.resetTo(resultState)
          }
      }
  }

  implicit class BinaryDeserializerZioPreludeOps(val obj: BinaryDeserializer.type) extends AnyVal {
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

  // ZIO prelude specific codecs

  implicit def nonEmptyChunkCodec[A: BinaryCodec]: BinaryCodec[NonEmptyChunk[A]] = {
    val inner = iterableCodec[A, Chunk[A]](BinaryCodec[A], Chunk.iterableFactory)
    BinaryCodec.from(
      inner.contramap(_.toChunk),
      new BinaryDeserializer[NonEmptyChunk[A]] {
        override def deserialize()(implicit ctx: DeserializationContext): NonEmptyChunk[A] = {
          val chunk = inner.deserialize()
          NonEmptyChunk.fromChunk(chunk) match {
            case Some(nonEmptyChunk) => nonEmptyChunk
            case None                =>
              failDeserializerWith(DesertFailure.DeserializationFailure("Non empty chunk is serialized as empty", None))
          }
        }
      }
    )
  }

  implicit def validationCodec[E: BinaryCodec, A: BinaryCodec]: BinaryCodec[Validation[E, A]] =
    new BinaryCodec[Validation[E, A]] {
      override def deserialize()(implicit ctx: DeserializationContext): Validation[E, A] = {
        val isValid = read[Boolean]()
        if (isValid)
          Validation.succeed(read[A]())
        else {
          val errors = read[NonEmptyChunk[E]]()
          Validation.validateAll(errors.map(Validation.fail(_))).map(_.asInstanceOf[A])
        }
      }

      override def serialize(value: Validation[E, A])(implicit context: SerializationContext): Unit =
        value match {
          case Validation.Failure(_, value) => write(false); write(value)
          case Validation.Success(_, value) => write(true); write(value)
        }
    }

  implicit def nonEmptyListCodec[A: BinaryCodec]: BinaryCodec[NonEmptyList[A]] = {
    val inner = listCodec[A]
    BinaryCodec.from(
      inner.contramap(_.toList),
      new BinaryDeserializer[NonEmptyList[A]] {
        override def deserialize()(implicit ctx: DeserializationContext): NonEmptyList[A] =
          inner.deserialize() match {
            case x :: xs => NonEmptyList.fromIterable(x, xs)
            case Nil     =>
              failDeserializerWith(DesertFailure.DeserializationFailure("Non empty list is serialized as empty", None))
          }
      }
    )
  }

  implicit def zsetCodec[A: BinaryCodec, B: BinaryCodec]: BinaryCodec[ZSet[A, B]] = {
    val inner = mapCodec[A, B]
    BinaryCodec.from(
      inner.contramap(_.toMap),
      inner.map(ZSet.fromMap)
    )
  }
}
