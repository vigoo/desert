---
layout: docs
title: Cats
---

# Cats integration

The cats module contains some additional codecs for [cats](https://typelevel.org/cats/) data types.

To use it add the dependency:
```scala
libraryDependencies += "io.github.vigoo" %% "desert-cats" % "0.3.0"
```

then import 

```scala mdoc
import cats.data._

import io.github.vigoo.desert._
import io.github.vigoo.desert.catssupport._
```

to get codecs for:

```scala
val valid = serializeToArray[Validated[String, Int]](Validated.Valid(100))
val invalid = serializeToArray[Validated[String, Int]](Validated.Invalid("error"))
val nel = serializeToArray(NonEmptyList.of(1, 2, 3, 4))
val nes = serializeToArray(NonEmptySet.of(1, 2, 3, 4))
val nem = serializeToArray(NonEmptyMap.of(1 -> "x", 2 -> "y"))
``` 