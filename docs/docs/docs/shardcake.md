---
layout: docs
title: Shardcake
---

# Shardcake integration

The shardcake makes it possible to use _desert_ as a [Shardcake](https://devsisters.github.io/shardcake/) serializer. 

To use it add the dependency:
```scala
libraryDependencies += "io.github.vigoo" %% "desert-shardcake" % "0.3.0"
```

### Usage

Create a [type registry](type-registry) with the types you want to serialize and then use `DesertSerialization`
as a shardcake serializer layer:

```scala mdoc
import io.github.vigoo.desert._
import io.github.vigoo.desert.shapeless._
import io.github.vigoo.desert.shardcakesupport._
import com.devsisters.shardcake.interfaces.Serialization
import zio._

case class Test(a: Int, b: String)

object Test {
  implicit val codec: BinaryCodec[Test] = DerivedBinaryCodec.derive
}

val serializerLayer: ULayer[Serialization] = 
  DesertSerialization.withTypeRegistry(
    DefaultTypeRegistry()
      .register[Test]
      .freeze()
    )
```
