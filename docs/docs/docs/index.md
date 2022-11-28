---
layout: docs
title: Getting started
---

# Getting started with desert

Desert is a _binary serialization library_ for Scala, focusing on creating small binaries 
while still enabling binary compatible evolution of the data model.
 
It is suitable for use cases such as Akka _remoting_ and _persistence_.

First add `desert` as a dependency:

```scala
libraryDependencies += "io.github.vigoo" %% "desert-core" % "0.3.0"
```

choose either the Shapeless based or the ZIO Schema based derivation implementation:

```scala
libraryDependencies += "io.github.vigoo" %% "desert-shapeless" % "0.3.0"
// or
libraryDependencies += "io.github.vigoo" %% "desert-zio-schema" % "0.3.0"
```

and optionally some extra bindings:

```scala
libraryDependencies += "io.github.vigoo" %% "desert-akka" % "0.3.0"
libraryDependencies += "io.github.vigoo" %% "desert-cats" % "0.3.0"
libraryDependencies += "io.github.vigoo" %% "desert-cats-effect" % "0.3.0"
libraryDependencies += "io.github.vigoo" %% "desert-zio" % "0.3.0"
libraryDependencies += "io.github.vigoo" %% "desert-shardcake" % "0.3.0"
```

The most simple use case is to serialize a known type to an array of bytes and read it back:

```scala mdoc:silent
import io.github.vigoo.desert._

val x = "Hello world"
```
```scala mdoc:serialized
val dataOrFailure = serializeToArray(x)
```

```scala mdoc
val y = dataOrFailure.flatMap(data => deserializeFromArray[String](data))
```

### Codecs

This works because there is an implicit `BinaryCodec` for `String` in scope, imported from the `desert` package. Read
the [codecs page](codecs) to learn about the available codecs and how to define custom ones.

### Low level input/output

The above example shows the convenient functions to work on arrays directly, but they have a more generic
version working on the low level `BinaryInput` and `BinaryOutput` interfaces. These are described on the [input/output page](input-output). 

### Evolution
One of the primary features of the library is the support for _evolving the data model_. The possibilities
are described on a [separate page](evolution).

### Type registry
For cases when the exact type to be deserialized is not known at compile type, the possibilities
 can be registered to a [type registry](type-registry).

### Integrations
There are three additional modules providing integrations to different environments:

- [Akka](akka) codecs and _Akka Serializer_ implementation
- [Cats](cats) adding some _Cats_ specific codecs
- [Cats Effect](cats-effect) defining the serialization/deserialization as an effect
- [Shardcake](shardcake) defines a _Shardcake_ serializer
- [ZIO](zio) defining the serialization/deserialization as an effect and some codecs

