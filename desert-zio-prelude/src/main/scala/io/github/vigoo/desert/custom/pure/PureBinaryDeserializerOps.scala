package io.github.vigoo.desert.custom.pure

import io.github.vigoo.desert.TypeRegistry.RegisteredTypeId
import io.github.vigoo.desert.custom.DeserializationContext
import io.github.vigoo.desert.internal.SerializerState.{RefId, StringId}
import io.github.vigoo.desert.internal.{DeserializationEnv, PureSerializerState, SerializerState}
import io.github.vigoo.desert.{
  BinaryCodec,
  BinaryDeserializer,
  BinaryInput,
  DesertException,
  DesertFailure,
  TypeRegistry,
  custom
}
import zio.prelude.fx.ZPure

import scala.util.Try

trait PureBinaryDeserializerOps {
  final def getInput: Deser[BinaryInput]                                  = ZPure.serviceWith(_.input)
  final def getInputTypeRegistry: Deser[TypeRegistry]                     = ZPure.serviceWith(_.typeRegistry)
  final def getDeserializationEnv: Deser[DeserializationEnv]              = ZPure.service
  final def getDeserializerState: Deser[PureSerializerState]              = ZPure.get
  final def setDeserializerState(state: PureSerializerState): Deser[Unit] = ZPure.set(state)

  final def readByte(): Deser[Byte]                              = getInput.flatMap(input => Deser.attempt(input.readByte()))
  final def readShort(): Deser[Short]                            = getInput.flatMap(input => Deser.attempt(input.readShort()))
  final def readInt(): Deser[Int]                                = getInput.flatMap(input => Deser.attempt(input.readInt()))
  final def readVarInt(optimizeForPositive: Boolean): Deser[Int] =
    getInput.flatMap(input => Deser.attempt(input.readVarInt(optimizeForPositive)))
  final def readLong(): Deser[Long]                              = getInput.flatMap(input => Deser.attempt(input.readLong()))
  final def readFloat(): Deser[Float]                            = getInput.flatMap(input => Deser.attempt(input.readFloat()))
  final def readDouble(): Deser[Double]                          = getInput.flatMap(input => Deser.attempt(input.readDouble()))
  final def readBytes(count: Int): Deser[Array[Byte]]            =
    getInput.flatMap(input => Deser.attempt(input.readBytes(count)))
  final def readCompressedByteArray(): Deser[Array[Byte]]        =
    getInput.flatMap(input => Deser.attempt(input.readCompressedByteArray()))

  final def read[T: BinaryDeserializer](): Deser[T] =
    for {
      state             <- getDeserializerState
      env               <- getDeserializationEnv
      pair              <- runUnsafeDeserializer(state, env, implicitly[BinaryDeserializer[T]])
      (result, newState) = pair
      _                 <- setDeserializerState(newState)
    } yield result

  private def runUnsafeDeserializer[T](
      state: PureSerializerState,
      env: DeserializationEnv,
      deserializer: BinaryDeserializer[T]
  ): Deser[(T, PureSerializerState)] = {
    val mutableState                         = SerializerState.create
    mutableState.resetTo(state)
    implicit val ctx: DeserializationContext = custom.DeserializationContext(env, mutableState)
    try {
      val result = deserializer.deserialize()
      finishDeserializerWith((result, mutableState.toPure))
    } catch {
      case DesertException(e) => ZPure.fail(e)
    }
  }

  final def readUnknown(): Deser[Any] =
    for {
      typeRegistry <- getInputTypeRegistry
      typeId       <- readVarInt(optimizeForPositive = true).map(RegisteredTypeId.apply)
      result       <- typeRegistry.forId(typeId) match {
                        case Some(registration) =>
                          for {
                            state             <- getDeserializerState
                            env               <- getDeserializationEnv
                            pair              <- runUnsafeDeserializer(state, env, registration.codec)
                            (result, newState) = pair
                            _                 <- setDeserializerState(newState)
                          } yield result
                        case None               =>
                          failDeserializerWith(DesertFailure.InvalidTypeId(typeId))
                      }
    } yield result

  final def finishDeserializerWith[T](value: T): Deser[T]                    = Deser.fromEither(Right(value))
  final def failDeserializerWith[T](failure: DesertFailure): Deser[T]        = Deser.fromEither(Left(failure))
  final def deserializerFromTry[T](f: Try[T], failMessage: String): Deser[T] =
    Deser.fromEither(f.toEither.left.map(failure => DesertFailure.DeserializationFailure(failMessage, Some(failure))))

  final def getString(value: StringId): Deser[Option[String]] =
    for {
      state <- getDeserializerState
    } yield state.stringsById.get(value)

  final def storeReadString(value: String): Deser[Unit] =
    for {
      state        <- getDeserializerState
      (newState, _) = state.storeString(value)
      _            <- setDeserializerState(newState)
    } yield ()

  final def getRef(value: RefId): Deser[Option[AnyRef]] =
    for {
      state <- getDeserializerState
    } yield state.refsById.get(value)

  final def storeReadRef(value: AnyRef): Deser[Unit] =
    for {
      state        <- getDeserializerState
      (newState, _) = state.storeRef(value)
      _            <- setDeserializerState(newState)
    } yield ()

  def readRefOrValue[T <: AnyRef](storeReadReference: Boolean = true)(implicit codec: BinaryCodec[T]): Deser[T] =
    readVarInt(optimizeForPositive = true).flatMap {
      case 0  =>
        for {
          value <- read[T]()
          _     <- if (storeReadReference) storeReadRef(value) else finishDeserializerWith(())
        } yield value
      case id =>
        getRef(RefId(id)).flatMap {
          case None        => failDeserializerWith(DesertFailure.InvalidRefId(RefId(id)))
          case Some(value) => finishDeserializerWith(value.asInstanceOf[T])
        }
    }
}
