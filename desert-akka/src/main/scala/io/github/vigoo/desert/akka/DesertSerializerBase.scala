package io.github.vigoo.desert.akka

import akka.serialization.Serializer
import io.github.vigoo.desert.syntax._
import io.github.vigoo.desert.{DesertException, TypeRegistry}

abstract class DesertSerializerBase extends Serializer {
  override val identifier: Int = 20551494
  override val includeManifest: Boolean = false

  val typeRegistry: TypeRegistry

  override def toBinary(o: AnyRef): Array[Byte] = {
    serializeUnknownToArray(o, typeRegistry) match {
      case Left(failure) => throw new DesertException(failure)
      case Right(bytes) => bytes
    }
  }

  override def fromBinary(bytes: Array[Byte], manifest: Option[Class[_]]): AnyRef = {
    deserializeUnknownFromArray(bytes, typeRegistry)
  }
}
