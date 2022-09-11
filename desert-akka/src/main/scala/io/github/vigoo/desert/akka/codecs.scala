package io.github.vigoo.desert.akka

import akka.actor.{ActorRef => UntypedActorRef, ExtendedActorSystem => UntypedExtendedActorSystem}
import akka.actor.typed.{ActorRef, ActorRefResolver, ActorSystem}
import akka.serialization.Serialization
import akka.util.ByteString
import io.github.vigoo.desert.BinaryCodec
import io.github.vigoo.desert.codecs._

object codecs {
  implicit def untypedActorRefCodec(implicit system: UntypedExtendedActorSystem): BinaryCodec[UntypedActorRef] =
    BinaryCodec.from(
      stringCodec.contramap(Serialization.serializedActorPath),
      stringCodec.map(system.provider.resolveActorRef)
    )

  implicit def typedActorRefCodec[T](implicit system: ActorSystem[_]): BinaryCodec[ActorRef[T]] = {
    val resolver = ActorRefResolver(system)
    BinaryCodec.from(
      stringCodec.contramap(resolver.toSerializationFormat),
      stringCodec.map(resolver.resolveActorRef)
    )
  }

  implicit val byteStringCodec: BinaryCodec[ByteString] = BinaryCodec.from(
    arrayCodec[Byte].contramap(_.toArray),
    arrayCodec[Byte].map(ByteString.fromArrayUnsafe)
  )
}
