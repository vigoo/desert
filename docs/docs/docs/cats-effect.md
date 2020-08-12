---
layout: docs
title: Cats Effect
permalink: docs/cats-effect/
---

# Cats Effect integration

The cats-effect module simply wraps the top-level serialization functions into effects.

To use it add the dependency:
```scala
libraryDependencies += "io.github.vigoo" %% "desert-cats-effect" % "0.1.3"
```

then import 

```scala mdoc
import io.github.vigoo.desert._
import io.github.vigoo.desert.codecs._
import io.github.vigoo.desert.catseffect.syntax._
```

to get variants like

```scala
def serializeToArray[F[_] : Sync, T: BinarySerializer](value: T, typeRegistry: TypeRegistry = TypeRegistry.empty): F[Array[Byte]]
``` 