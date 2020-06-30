package io.github.vigoo.desert

import cats.data.{ReaderT, StateT}
import cats.instances.either._
import io.github.vigoo.desert.BinaryDeserializer.Deser
import io.github.vigoo.desert.BinarySerializer.Ser
import io.github.vigoo.desert.SerializerState.{StoreStringResult, StringId}

import scala.language.experimental.macros

trait BinarySerializer[T] { self =>
  import BinarySerializer._

  def serialize(value: T): Ser[Unit]

  def contramap[U](f: U => T): BinarySerializer[U] = (value: U) => self.serialize(f(value))
}

object BinarySerializer {
  type Ser[T] = ReaderT[StateT[Either[DesertFailure, *], SerializerState, *], BinaryOutput, T]

  object Ser {
    final def fromEither[T](value: Either[DesertFailure, T]): Ser[T] = ReaderT.liftF(StateT.liftF(value))
  }
}

trait BinarySerializerOps {
  final def getOutput: Ser[BinaryOutput] = ReaderT.ask[StateT[Either[DesertFailure, *], SerializerState, *], BinaryOutput]
  final def getSerializerState: Ser[SerializerState] = ReaderT.liftF(StateT.get[Either[DesertFailure, *], SerializerState])
  final def setSerializerState(state: SerializerState): Ser[Unit] = ReaderT.liftF(StateT.set[Either[DesertFailure, *], SerializerState](state))

  final def writeByte(value: Byte): Ser[Unit] = getOutput.flatMap(output => Ser.fromEither(output.writeByte(value)))
  final def writeShort(value: Short): Ser[Unit] = getOutput.flatMap(output => Ser.fromEither(output.writeShort(value)))
  final def writeInt(value: Int): Ser[Unit] = getOutput.flatMap(output => Ser.fromEither(output.writeInt(value)))
  final def writeVarInt(value: Int, optimizeForPositive: Boolean): Ser[Unit] = getOutput.flatMap(output => Ser.fromEither(output.writeVarInt(value, optimizeForPositive)))
  final def writeLong(value: Long): Ser[Unit] = getOutput.flatMap(output => Ser.fromEither(output.writeLong(value)))
  final def writeFloat(value: Float): Ser[Unit] = getOutput.flatMap(output => Ser.fromEither(output.writeFloat(value)))
  final def writeDouble(value: Double): Ser[Unit] = getOutput.flatMap(output => Ser.fromEither(output.writeDouble(value)))
  final def writeBytes(value: Array[Byte]): Ser[Unit] = getOutput.flatMap(output => Ser.fromEither(output.writeBytes(value)))
  final def writeUnknown(value: Any, typeRegistry: TypeRegistry): Ser[Unit] = getOutput.flatMap(output => Ser.fromEither(output.writeUnknown(value, typeRegistry)))
  final def write[U : BinarySerializer](value: U): Ser[Unit] = implicitly[BinarySerializer[U]].serialize(value)

  final def finishSerializer(): Ser[Unit] = finishSerializerWith(())
  final def finishSerializerWith[T](value: T): Ser[T] = Ser.fromEither(Right(value))
  final def failSerializerWith(failure: DesertFailure): Ser[Unit] = Ser.fromEither(Left(failure))

  final def storeString(value: String): Ser[StoreStringResult] =
    for {
      state <- getSerializerState
      (newState, result) = state.storeString(value)
      _ <- ReaderT.liftF(StateT.set[Either[DesertFailure, *], SerializerState](newState))
    } yield result
}

object BinarySerializerOps extends BinarySerializerOps

trait BinaryDeserializer[T] { self =>
  import BinaryDeserializer._

  def deserialize(): Deser[T]

  def map[U](f: T => U): BinaryDeserializer[U] = () => self.deserialize().map(f)
}

object BinaryDeserializer {
  type Deser[T] = ReaderT[StateT[Either[DesertFailure, *], SerializerState, *], BinaryInput, T]

  object Deser {
    final def fromEither[T](value: Either[DesertFailure, T]): Deser[T] = ReaderT.liftF(StateT.liftF(value))
  }
}

trait BinaryDeserializerOps {
  final def getInput: Deser[BinaryInput] = ReaderT.ask[StateT[Either[DesertFailure, *], SerializerState, *], BinaryInput]
  final def getDeserializerState: Deser[SerializerState] = ReaderT.liftF(StateT.get[Either[DesertFailure, *], SerializerState])
  final def setDeserializerState(state: SerializerState): Deser[Unit] = ReaderT.liftF(StateT.set[Either[DesertFailure, *], SerializerState](state))

  final def readByte(): Deser[Byte] = getInput.flatMap(input => Deser.fromEither(input.readByte()))
  final def readShort(): Deser[Short] = getInput.flatMap(input => Deser.fromEither(input.readShort()))
  final def readInt(): Deser[Int] = getInput.flatMap(input => Deser.fromEither(input.readInt()))
  final def readVarInt(optimizeForPositive: Boolean): Deser[Int] = getInput.flatMap(input => Deser.fromEither(input.readVarInt(optimizeForPositive)))
  final def readLong(): Deser[Long] = getInput.flatMap(input => Deser.fromEither(input.readLong()))
  final def readFloat(): Deser[Float] = getInput.flatMap(input => Deser.fromEither(input.readFloat()))
  final def readDouble(): Deser[Double] = getInput.flatMap(input => Deser.fromEither(input.readDouble()))
  final def readBytes(count: Int): Deser[Array[Byte]] = getInput.flatMap(input => Deser.fromEither(input.readBytes(count)))
  final def read[T: BinaryDeserializer](): Deser[T] = implicitly[BinaryDeserializer[T]].deserialize()

  final def finishDeserializerWith[T](value: T): Deser[T] = Deser.fromEither(Right(value))
  final def failDeserializerWith[T](failure: DesertFailure): Deser[T] = Deser.fromEither(Left(failure))

  final def getString(value: StringId): Deser[Option[String]] =
    for {
      state <- getDeserializerState
    } yield state.stringsById.get(value)

  final def storeReadString(value: String): Deser[Unit] =
    for {
      state <- ReaderT.liftF(StateT.get[Either[DesertFailure, *], SerializerState])
      (newState, _) = state.storeString(value)
      _ <- ReaderT.liftF(StateT.set[Either[DesertFailure, *], SerializerState](newState))
    } yield ()
}

object BinaryDeserializerOps extends BinaryDeserializerOps

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

  def deriveF[T](evolutionSteps: Evolution*)(f: GenericDerivationApi => BinaryCodec[T]): BinaryCodec[T] =
    if (evolutionSteps.isEmpty) {
      f(GenericBinaryCodec.simple)
    } else {
      f(new GenericBinaryCodec(InitialVersion +: evolutionSteps.toVector))
    }
}
