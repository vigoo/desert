---
layout: docs
title: Type Registry
permalink: docs/type-registry/
---

# Type Registry
There are cases when the exact type to be deserialized is not known at compile time. 
In this case an additional _type ID_ must be serialized to the data stream in order to
being able to select the correct deserializer when reading it back.

`desert` is not doing any automatic reflection based type identification. All the types that
can participate in the above described scenario must be explicitly registered to a _type registry_:

```scala mdoc silent
import io.github.vigoo.desert._
import io.github.vigoo.desert.codecs._
import io.github.vigoo.desert.syntax._

case class TestProd(value: Int)
object TestProd {
  implicit val codec: BinaryCodec[TestProd] = BinaryCodec.derive()
}
```

```scala mdoc
val typeRegistry1 = DefaultTypeRegistry()
  .register[String]
  .register[TestProd]
  .freeze() 
```

In this example we register two types, `String` and a custom case class `TestProd`. We can then use
this registry to try serializing a value of `AnyRef`. If the type matches any of the registered ones,
it will succeed:

```scala mdoc
val dataOrFailure1 = serializeUnknownToArray("Hello world", typeRegistry1)
val dataOrFailure2 = serializeUnknownToArray(TestProd(11), typeRegistry1)
```

Both data streams start with a compact type ID (in this case it is `1` and `2`) so when deserializing
them, the library knows which codec to use:

```scala mdoc
val x = dataOrFailure1.flatMap(data1 => deserializeUnknownFromArray(data1, typeRegistry1))
val y = dataOrFailure2.flatMap(data2 => deserializeUnknownFromArray(data2, typeRegistry1))
``` 

### Unknowns in fields

These functions (and the similar ones working on `BinaryInput` and `BinaryOutput` instances) are good
when the top-level type is unknown, like when serializing arbitrary actor messages.

But what if a field's type is an open trait? For this the `BinaryCodec.unknown` function
can be used to automatically use the above described mechanisms.

Let's see an example:

```scala mdoc silent
trait Iface
object Iface {
  implicit val codec: BinaryCodec[Iface] = BinaryCodec.unknown
}

case class Impl1(x: Int) extends Iface
object Impl1 {
  implicit val codec: BinaryCodec[Impl1] = BinaryCodec.derive()
}

case class Impl2(x: Int) extends Iface
object Impl2 {
  implicit val codec: BinaryCodec[Impl2] = BinaryCodec.derive()
}

case class Outer(inner: Iface)
object Outer {
  implicit val codec: BinaryCodec[Outer] = BinaryCodec.derive()
}

val typeRegistry2 = DefaultTypeRegistry()
  .register[Impl1]
  .register[Impl2]
  .freeze()
```

```scala mdoc
val dataOrFailure = serializeToArray(Outer(Impl2(11)), typeRegistry2)
val result = dataOrFailure.flatMap(data => deserializeFromArray[Outer](data, typeRegistry2))
```  
