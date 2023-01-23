package io.github.vigoo.desert.custom.pure

import io.github.vigoo.desert.custom.SerializationContext
import io.github.vigoo.desert.internal.SerializerState.{RefAlreadyStored, RefIsNew, StoreRefResult, StoreStringResult}
import io.github.vigoo.desert.internal.{PureSerializerState, SerializationEnv, SerializerState}
import io.github.vigoo.desert.{
  BinaryCodec,
  BinaryOutput,
  BinarySerializer,
  DesertException,
  DesertFailure,
  TypeRegistry,
  custom
}
import zio.prelude.fx.ZPure

import java.util.zip.Deflater
import scala.util.Try

trait PureBinarySerializerOps {
  final def getOutput: Ser[BinaryOutput]                              = ZPure.serviceWith(_.output)
  final def getOutputTypeRegistry: Ser[TypeRegistry]                  = ZPure.serviceWith(_.typeRegistry)
  final def getSerializationEnv: Ser[SerializationEnv]                = ZPure.service
  final def getSerializerState: Ser[PureSerializerState]              = ZPure.get
  final def setSerializerState(state: PureSerializerState): Ser[Unit] = ZPure.set(state)

  final def writeByte(value: Byte): Ser[Unit]                                                     = getOutput.flatMap(output => Ser.attempt(output.writeByte(value)))
  final def writeShort(value: Short): Ser[Unit]                                                   = getOutput.flatMap(output => Ser.attempt(output.writeShort(value)))
  final def writeInt(value: Int): Ser[Unit]                                                       = getOutput.flatMap(output => Ser.attempt(output.writeInt(value)))
  final def writeVarInt(value: Int, optimizeForPositive: Boolean): Ser[Unit]                      =
    getOutput.flatMap(output => Ser.attempt(output.writeVarInt(value, optimizeForPositive)))
  final def writeLong(value: Long): Ser[Unit]                                                     = getOutput.flatMap(output => Ser.attempt(output.writeLong(value)))
  final def writeFloat(value: Float): Ser[Unit]                                                   = getOutput.flatMap(output => Ser.attempt(output.writeFloat(value)))
  final def writeDouble(value: Double): Ser[Unit]                                                 =
    getOutput.flatMap(output => Ser.attempt(output.writeDouble(value)))
  final def writeBytes(value: Array[Byte]): Ser[Unit]                                             =
    getOutput.flatMap(output => Ser.attempt(output.writeBytes(value)))
  final def writeCompressedBytes(value: Array[Byte], level: Int = Deflater.BEST_SPEED): Ser[Unit] =
    getOutput.flatMap(output => Ser.attempt(output.writeCompressedByteArray(value, level)))

  final def writeUnknown(value: Any): Ser[Unit] =
    getOutputTypeRegistry.flatMap { typeRegistry =>
      typeRegistry.get(value) match {
        case Some(registration) =>
          for {
            _        <- writeVarInt(registration.id.value, optimizeForPositive = true)
            state    <- getSerializerState
            env      <- getSerializationEnv
            newState <- runUnsafeSerializer(state, env, registration.codec.asInstanceOf[BinarySerializer[Any]], value)
            _        <- setSerializerState(newState)
          } yield ()
        case None               =>
          failSerializerWith(DesertFailure.TypeNotRegistered(value.getClass))
      }
    }

  final def write[U: BinarySerializer](value: U): Ser[Unit] =
    for {
      state    <- getSerializerState
      env      <- getSerializationEnv
      newState <- runUnsafeSerializer(state, env, implicitly[BinarySerializer[U]], value)
      _        <- setSerializerState(newState)
    } yield ()

  private def runUnsafeSerializer[T](
      state: PureSerializerState,
      env: SerializationEnv,
      serializer: BinarySerializer[T],
      value: T
  ): Ser[PureSerializerState] = {
    val mutableState                       = SerializerState.create
    mutableState.resetTo(state)
    implicit val ctx: SerializationContext = custom.SerializationContext(env, mutableState)
    try {
      serializer.serialize(value)
      finishSerializerWith(mutableState.toPure)
    } catch {
      case DesertException(e) => ZPure.fail(e)
    }
  }

  final def finishSerializer(): Ser[Unit]                                  = finishSerializerWith(())
  final def finishSerializerWith[T](value: T): Ser[T]                      = Ser.fromEither(Right(value))
  final def failSerializerWith(failure: DesertFailure): Ser[Unit]          = Ser.fromEither(Left(failure))
  final def serializerFromTry[T](f: Try[T], failMessage: String): Deser[T] =
    Deser.fromEither(f.toEither.left.map(failure => DesertFailure.SerializationFailure(failMessage, Some(failure))))

  final def storeString(value: String): Ser[StoreStringResult] =
    for {
      state             <- getSerializerState
      (newState, result) = state.storeString(value)
      _                 <- setSerializerState(newState)
    } yield result

  final def storeRef(value: AnyRef): Ser[StoreRefResult] =
    for {
      state             <- getSerializerState
      (newState, result) = state.storeRef(value)
      _                 <- setSerializerState(newState)
    } yield result

  final def storeRefOrObject[T <: AnyRef](value: T)(implicit codec: BinaryCodec[T]): Ser[Unit] =
    storeRef(value).flatMap {
      case RefAlreadyStored(id) => writeVarInt(id.value, optimizeForPositive = true)
      case RefIsNew(_)          => writeVarInt(0, optimizeForPositive = true) *> write(value)
    }
}
