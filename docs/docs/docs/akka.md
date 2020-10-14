---
layout: docs
title: Akka
permalink: docs/akka/
---

# Akka integration
The akka module adds some extra codecs and an _akka serializer_.

To use it add the dependency:
```scala
libraryDependencies += "io.github.vigoo" %% "desert-akka" % "0.2.0"
``` 

### Codecs
The following codecs are added:

- Codec for *Akka Classic actor references* (`BinaryCodec[ActorRef]`)
- Codec for *Akka typed actor references* (`BinaryCodec[ActorRef[T]]`)
- Codec for `ByteString`

### Syntax
Some akka-specific syntax has been added to `io.github.vigoo.desert.akka.syntax._`:

- `serializeToByteString`
- `serializeUnknownToByteString`
- `deserializeFromByteString`
- `deserializeUnknownFromByteString`

### Akka Serialization
`desert` can be used as an Akka serializer for remoting and persistence by inheriting from `DesertSerializerBase` and setting up a [type registry](type-registry) in it.

```scala mdoc
import io.github.vigoo.desert._
import io.github.vigoo.desert.codecs._
import io.github.vigoo.desert.akka._

class DesertSerializer extends DesertSerializerBase {
  override val typeRegistry: TypeRegistry = DefaultTypeRegistry()
    // .register[X]()
    // ...
    // .register[Z]()
    .freeze()
}
```

