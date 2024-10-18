package io.github.vigoo.desert.pekkosupport

import org.apache.pekko
import pekko.actor.{ActorRef => UntypedActorRef, ExtendedActorSystem => UntypedExtendedActorSystem}
import pekko.actor.typed.{ActorRef, ActorRefResolver, ActorSystem}
import pekko.serialization.Serialization
import pekko.util.ByteString
import io.github.vigoo.desert._

trait Codecs {
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
