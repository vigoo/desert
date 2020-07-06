package io.github.vigoo.desert

import io.github.vigoo.desert.BinaryDeserializer.Deser
import io.github.vigoo.desert.BinarySerializer.Ser
import shapeless.Lazy
import zio._

import scala.language.experimental.macros
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

trait BinarySerializer[T] { self =>
  import BinarySerializer._

  def serialize(value: T): Ser[Unit]

  def contramap[U](f: U => T): BinarySerializer[U] = (value: U) => self.serialize(f(value))
}

object BinarySerializer {
  final case class SerializationEnv(output: BinaryOutput, typeRegistry: TypeRegistry, state: Ref[SerializerState])
  object SerializationEnv {
    def create(output: BinaryOutput, typeRegistry: TypeRegistry): ZIO[Any, Nothing, SerializationEnv] =
      for {
        stateRef <- Ref.make(SerializerState.initial)
      } yield SerializationEnv(output, typeRegistry, stateRef)
  }

  type Ser[T] = ZIO[SerializationEnv, DesertFailure, T]
}

trait BinaryDeserializer[T] { self =>
  import BinaryDeserializer._

  def deserialize(): Deser[T]

  def map[U](f: T => U): BinaryDeserializer[U] = () => self.deserialize().map(f)
}

object BinaryDeserializer {
  final case class DeserializationEnv(input: BinaryInput, typeRegistry: TypeRegistry, state: Ref[SerializerState])
  object DeserializationEnv {
    def create(input: BinaryInput, typeRegistry: TypeRegistry): ZIO[Any, Nothing, DeserializationEnv] =
      for {
        stateRef <- Ref.make(SerializerState.initial)
      } yield DeserializationEnv(input, typeRegistry, stateRef)
  }

  type Deser[T] = ZIO[DeserializationEnv, DesertFailure, T]
}

trait BinaryCodec[T] extends BinarySerializer[T] with BinaryDeserializer[T]

object BinaryCodec {
  implicit def from[T](serializer: BinarySerializer[T], deserializer: BinaryDeserializer[T]): BinaryCodec[T] = new BinaryCodec[T] {
    override def deserialize(): Deser[T] = deserializer.deserialize()
    override def serialize(value: T): Ser[Unit] = serializer.serialize(value)
  }

  def define[T](serializeFn: T => Ser[Unit])(deserializeFn: Deser[T]): BinaryCodec[T] = new BinaryCodec[T] {
    override def serialize(value: T): Ser[Unit] = serializeFn(value)
    override def deserialize(): Deser[T] = deserializeFn
  }

  def derive[T](evolutionSteps: Evolution*): BinaryCodec[T] = macro Macros.deriveImpl[T]

  def deriveForWrapper[T](implicit codec: Lazy[UnwrappedBinaryCodec[T]]): BinaryCodec[T] = codec.value

  def deriveF[T](evolutionSteps: Evolution*)(f: GenericDerivationApi => BinaryCodec[T]): BinaryCodec[T] =
    if (evolutionSteps.isEmpty) {
      f(GenericBinaryCodec.simple)
    } else {
      f(new GenericBinaryCodec(InitialVersion +: evolutionSteps.toVector))
    }

  def unknown[T](implicit tag: ClassTag[T]): BinaryCodec[T] =
    define[T](
      BinarySerializerOps.writeUnknown
    )(
      BinaryDeserializerOps.readUnknown().flatMap { value =>
        Try(value.asInstanceOf[T]) match {
          case Success(upcasted) => BinaryDeserializerOps.finishDeserializerWith(upcasted)
          case Failure(exception) => BinaryDeserializerOps.failDeserializerWith(SerializationUpcastError(value.getClass, tag.runtimeClass, exception))
        }
      }
    )
}
