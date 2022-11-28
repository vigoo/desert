package io.github.vigoo

import io.github.vigoo.desert.internal.{DeserializationEnv, SerializationEnv, SerializerState}
import zio.prelude.fx.ZPure

package object desert extends BinarySerialization with Codecs {

  type Ser[T] = ZPure[Nothing, SerializerState, SerializerState, SerializationEnv, DesertFailure, T]

  object Ser {
    final def fromEither[T](value: Either[DesertFailure, T]): Ser[T] =
      ZPure.succeed(value).absolve
  }

  type Deser[T] = ZPure[Nothing, SerializerState, SerializerState, DeserializationEnv, DesertFailure, T]

  object Deser {
    final def fromEither[T](value: Either[DesertFailure, T]): Deser[T] =
      ZPure.succeed(value).absolve
  }
}
