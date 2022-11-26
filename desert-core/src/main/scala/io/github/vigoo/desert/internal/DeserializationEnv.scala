package io.github.vigoo.desert.internal

import io.github.vigoo.desert.{BinaryInput, TypeRegistry}

final case class DeserializationEnv(input: BinaryInput, typeRegistry: TypeRegistry)
