package io.github.vigoo.desert.custom

import io.github.vigoo.desert.internal.{DeserializationEnv, PureSerializerState, SerializationEnv}
import io.github.vigoo.desert.{DesertException, DesertFailure}
import zio.prelude.fx.ZPure

package object pure extends PureBinarySerializerOps with PureBinaryDeserializerOps {

  type Ser[T] = ZPure[Nothing, PureSerializerState, PureSerializerState, SerializationEnv, DesertFailure, T]

  object Ser {
    final def fromEither[T](value: Either[DesertFailure, T]): Ser[T] =
      ZPure.succeed(value).absolve

    final def attempt[T](value: => T): Ser[T] =
      try
        ZPure.succeed(value)
      catch {
        case DesertException(e) => ZPure.fail(e)
      }
  }

  type Deser[T] = ZPure[Nothing, PureSerializerState, PureSerializerState, DeserializationEnv, DesertFailure, T]

  object Deser {
    final def fromEither[T](value: Either[DesertFailure, T]): Deser[T] =
      ZPure.succeed(value).absolve

    final def attempt[T](value: => T): Deser[T] =
      try ZPure.succeed(value)
      catch {
        case DesertException(e) => ZPure.fail(e)
      }
  }
}
