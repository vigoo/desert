package io.github.vigoo.desert.custom

import io.github.vigoo.desert.internal.{DeserializationEnv, SerializerState}

final case class DeserializationContext(env: DeserializationEnv, state: SerializerState)
