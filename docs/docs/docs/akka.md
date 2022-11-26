---
layout: docs
title: Akka
---

# Akka integration
The akka module adds some extra codecs and an _akka serializer_.

To use it add the dependency:
```scala
libraryDependencies += "io.github.vigoo" %% "desert-akka" % "0.3.0"
``` 

### Codecs
The module defines the following codecs:

- Codec for *Akka Classic actor references* (`BinaryCodec[ActorRef]`)
- Codec for *Akka typed actor references* (`BinaryCodec[ActorRef[T]]`)
- Codec for `ByteString`

### Syntax
Some akka-specific serializer functions are exposed in the`io.github.vigoo.desert.akkasupport` package:

- `serializeToByteString`
- `serializeUnknownToByteString`
- `deserializeFromByteString`
- `deserializeUnknownFromByteString`

### Akka Serialization
`desert` can be used as an Akka serializer for remoting and persistence by inheriting from `DesertSerializerBase` and setting up a [type registry](type-registry) in it.

```scala mdoc
import io.github.vigoo.desert._
import io.github.vigoo.desert.akkasupport._

class DesertSerializer extends DesertSerializerBase {
  override val typeRegistry: TypeRegistry = DefaultTypeRegistry()
    // .register[X]()
    // ...
    // .register[Z]()
    .freeze()
}
```

