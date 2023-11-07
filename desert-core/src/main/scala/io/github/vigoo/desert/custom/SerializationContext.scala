package io.github.vigoo.desert.custom

import io.github.vigoo.desert.internal.{SerializationEnv, SerializerState}

final case class SerializationContext(env: SerializationEnv, state: SerializerState)
