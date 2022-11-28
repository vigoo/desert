---
layout: docs
title: ZIO
---

# ZIO integration

The zio module contains codecs for the ZIO `Chunk` type and wraps the top level serialization functions
in ZIO effects.  

To use it add the dependency:
```scala
libraryDependencies += "io.github.vigoo" %% "desert-zio" % "0.3.0"
```

### API

Import the zio-specific interface: 

```scala mdoc
import io.github.vigoo.desert._
import io.github.vigoo.desert.ziosupport._
```

to get variants like

```scala
def serializeToChunk[T: BinarySerializer](value: T, typeRegistry: TypeRegistry = TypeRegistry.empty): ZIO[Any, DesertFailure, Chunk[Byte]]
``` 

### Codecs
There is a generic `Chunk` codec built on top of `iterableCodec` so it is binary compatible with other collections.

It is also specialized for `Chunk[Byte]` so it directly uses the input/output interfaces 'readBytes' and 'writeBytes' 
functions for higher performance.
