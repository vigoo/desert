package io.github.vigoo.desert.shardcakesupport

import com.devsisters.shardcake.interfaces.Serialization
import io.github.vigoo.desert.ziosupport.{deserializeUnknownFromArray, serializeUnknownToArray}
import io.github.vigoo.desert.{DesertException, TypeRegistry}
import zio.{Task, ULayer, ZLayer}

object DesertSerialization {
  def withTypeRegistry(typeRegistry: TypeRegistry): ULayer[Serialization] =
    ZLayer.succeed {
      new Serialization {
        override def encode(message: Any): Task[Array[Byte]] =
          serializeUnknownToArray(message, typeRegistry).mapError(new DesertException(_))

        override def decode[A](bytes: Array[Byte]): Task[A] =
          deserializeUnknownFromArray(bytes, typeRegistry).mapBoth(
            new DesertException(_),
            _.asInstanceOf[A]
          )
      }
    }
}
