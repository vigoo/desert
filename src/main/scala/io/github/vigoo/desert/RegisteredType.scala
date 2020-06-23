package io.github.vigoo.desert

import io.github.vigoo.desert.TypeRegistry.RegisteredTypeId

case class RegisteredType[T](id: RegisteredTypeId, codec: BinaryCodec[T])
