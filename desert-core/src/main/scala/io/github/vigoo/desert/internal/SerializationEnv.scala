package io.github.vigoo.desert.internal

import io.github.vigoo.desert.{BinaryOutput, TypeRegistry}

final case class SerializationEnv(output: BinaryOutput, typeRegistry: TypeRegistry)
