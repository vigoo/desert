---
layout: docs
title: Cats Effect
---

# Cats Effect integration

The cats-effect module simply wraps the top-level serialization functions into effects.

To use it add the dependency:
```scala
libraryDependencies += "io.github.vigoo" %% "desert-cats-effect" % "0.3.0"
```

then import 

```scala mdoc
import io.github.vigoo.desert.{TypeRegistry, BinarySerializer}
import io.github.vigoo.desert.catseffect._
```

to get variants like

```scala
def serializeToArray[F[_] : Sync, T: BinarySerializer](value: T, typeRegistry: TypeRegistry = TypeRegistry.empty): F[Array[Byte]]
``` 