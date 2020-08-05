---
layout: docs
title: Getting started
permalink: docs/
---

# Getting started with desert

Desert is a _binary serialization library_ for Scala, focusing on creating small binaries 
while still enabling binary compatible evolution of the data model.
 
It is suitable for use cases such as Akka _remoting_ and _persistence_.

First add `desert` as a dependency:

```scala
libraryDependencies += "io.github.vigoo" %% "desert-core" % "0.1.2"
```

and optionally some extra bindings:

```scala
libraryDependencies += "io.github.vigoo" %% "desert-akka" % "0.1.2"
libraryDependencies += "io.github.vigoo" %% "desert-cats-effect" % "0.1.2"
libraryDependencies += "io.github.vigoo" %% "desert-zio" % "0.1.2"
```

The most simple use case is to serialize a known type to an array of bytes and read it back:

```scala mdoc:silent
import io.github.vigoo.desert._
import io.github.vigoo.desert.codecs._
import io.github.vigoo.desert.syntax._

val x = "Hello world"
```
```scala mdoc
val dataOrFailure = serializeToArray(x)

val y = dataOrFailure.flatMap(data => deserializeFromArray[String](data))
```

### Codecs

This works because there is an implicit `BinaryCodec` for `String` in scope, imported from the `codecs` package. Read
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
- [Cats Effect](cats-effect) defining the serialization/deserialization as an effect
- [ZIO](zio) defining the serialization/deserialization as an effect and some codecs

