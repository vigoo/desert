---
layout: docs
title: ZIO Prelude
---

# ZIO Prelude integration

The ZIO Prelude module contains codecs for some ZIO Prelude types and defines a functional `ZPure` based API for
defining codecs on top of the core low level one.

To use it add the dependency:

```scala
libraryDependencies += "io.github.vigoo" %% "desert-zio-prelude" % "0.3.0"
```

### Codecs

Import the zio prelude codecs from:

```scala mdoc
import io.github.vigoo.desert.zioprelude._
```

### ZPure API

The `ZPure` equivalent of the serializer/deserializer functions are defined in the `io.github.vigoo.desert.custom.pure`
package.

Two extension methods are added to the `BinarySerializer` and `BinaryDeserializer` companion objects by importing
the `zioprelude` package:

```scala
def fromPure[T](serializer: T => Ser[T]): BinarySerializer[T]
def fromPure[T](deserializer: Deser[T]): BinaryDeserializer[T]
```
