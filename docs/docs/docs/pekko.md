---
layout: docs
title: Pekko
---

# Pekko integration
The pekko module adds some extra codecs and a _pekko serializer_.

To use it add the dependency:
```scala
libraryDependencies += "io.github.vigoo" %% "desert-pekko" % "0.3.7"
``` 

### Codecs
The module defines the following codecs:

- Codec for *Pekko Classic actor references* (`BinaryCodec[ActorRef]`)
- Codec for *Pekko typed actor references* (`BinaryCodec[ActorRef[T]]`)
- Codec for `ByteString`

### Syntax
Some pekko-specific serializer functions are exposed in the`io.github.vigoo.desert.pekkosupport` package:

- `serializeToByteString`
- `serializeUnknownToByteString`
- `deserializeFromByteString`
- `deserializeUnknownFromByteString`

### Pekko Serialization
`desert` can be used as an Pekko serializer for remoting and persistence by inheriting from `DesertSerializerBase` and setting up a [type registry](type-registry) in it.

```scala mdoc
import io.github.vigoo.desert._
import io.github.vigoo.desert.pekkosupport._

class DesertSerializer extends DesertSerializerBase {
  override val typeRegistry: TypeRegistry = DefaultTypeRegistry()
    // .register[X]()
    // ...
    // .register[Z]()
    .freeze()
}
```
