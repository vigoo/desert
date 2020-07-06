package io.github.vigoo.desert

import zio.BootstrapRuntime

object syntax
  extends BinarySerialization
  with BinarySerializerOps
  with BinaryDeserializerOps
  with BootstrapRuntime
