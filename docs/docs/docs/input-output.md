---
layout: docs
title: Input/output
permalink: docs/input-output/
---

# Binary input/output
The `desert` library uses the `BinaryInput` and `BinaryOutput` interfaces to read and write data.
The default implementation is `JavaStreamBinaryInput` and `JavaStreamBinaryOutput`. These are simple
wrappers on top of Java IO streams, capturing failures and adding some extra functionality.

### Variable length integer encoding
One extra feature is the _variable length integer encoding_ which was borrowed from the [Kryo](https://github.com/EsotericSoftware/kryo)
library.

It encodes 32 bit integers in 1-5 bytes. It is used for all "integer id like" data within the library.

```scala mdoc
import java.io.ByteArrayOutputStream
import io.github.vigoo.desert._

val stream = new ByteArrayOutputStream()
val output = new JavaStreamBinaryOutput(stream)
output.writeVarInt(1, optimizeForPositive = true)
val r1 = stream.toByteArray

stream.reset()
output.writeVarInt(4096, optimizeForPositive = true)
val r2 = stream.toByteArray

stream.reset()
output.writeVarInt(-1, optimizeForPositive = true)
val r3 = stream.toByteArray
```

### [Z]IO? 
The read and write operations on these interfaces are not encapsulated to any effect type like Cats Effect's IO or ZIO,
they are just `Either[DesertFailure, T]`. This is for performance reasons and also for being independent of the chosen 
library at this low level. On the other hand the [Cats Effect module](cats-effect) and the [ZIO module](zio) both define
IO versions of the higher level serialization/deserialization functions. Usage of `desert` should be in low level
enough to treat these operations as a single effect.
