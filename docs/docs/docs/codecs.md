---
layout: docs
title: Codecs
permalink: docs/codecs/
---

# Codecs

A `BinaryCodec[T]` defines both the _serializer_ and _deserializer_ for a given type:

```scala
trait BinaryCodec[T] extends BinarySerializer[T] with BinaryDeserializer[T]
```
### Primitive types
The `io.github.vigoo.desert.codecs` module defines a lot of implicit binary codecs for common types.

The following code examples demonstrate this and also shows how the binary representation looks like.

```scala mdoc
import io.github.vigoo.desert.{BinaryCodec, transientConstructor, transientField}
import io.github.vigoo.desert.codecs._
import io.github.vigoo.desert.syntax._
import io.github.vigoo.desert.shapeless._

val byte = serializeToArray(100.toByte)
val short = serializeToArray(100.toShort)
val int = serializeToArray(100)
val long = serializeToArray(100L)
val float = serializeToArray(3.14.toFloat)
val double = serializeToArray(3.14)
val bool = serializeToArray(true)
val unit = serializeToArray(())
val str = serializeToArray("Hello")
val uuid = serializeToArray(java.util.UUID.randomUUID())
``` 

### Option, Either, Try, Validation
Common types such as `Option` and `Either` are also supported out of the box. For `Try` it
also has a codec for arbitrary `Throwable` instances, although deserializing it does not recreate
the original throwable just a `PersistedThrowable` instance. In practice this is a much safer approach
than trying to recreate the same exception via reflection.

```scala mdoc
import scala.collection.immutable.SortedSet
import scala.util._
import zio.NonEmptyChunk
import zio.prelude.Validation

val none = serializeToArray[Option[Int]](None)
val some = serializeToArray[Option[Int]](Some(100))
val left = serializeToArray[Either[Boolean, Int]](Left(true))
val right = serializeToArray[Either[Boolean, Int]](Right(100))
val valid = serializeToArray[Validation[String, Int]](Validation.succeed(100))
val invalid = serializeToArray[Validation[String, Int]](Validation.failNonEmptyChunk(NonEmptyChunk("error")))
```

```scala mdoc:silent
val fail = serializeToArray[Try[Int]](Failure(new RuntimeException("Test exception")))
```

```scala mdoc
val failDeser = fail.flatMap(data => deserializeFromArray[Try[Int]](data))
val success = serializeToArray[Try[Int]](Success(100))
```

### Collections
There is a generic `iterableCodec` that can be used to define implicit collection codecs based on
the Scala 2.13 collection API. For example this is how the `vectorCodec` is defined:

```scala
implicit def vectorCodec[A : BinaryCodec]: BinaryCodec[Vector[A]] = iterableCodec[A, Vector[A]]
```

All these collection codecs have one of the two possible representation. If the size is known in advance
then it is the number of elements followed by all the items in iteration order, otherwise it is a flat
list of all the elements wrapped in `Option[T]`. `Vector` and `List` are good examples for the two:

```scala mdoc
val vec = serializeToArray(Vector(1, 2, 3, 4))
val lst = serializeToArray(List(1, 2, 3, 4))
```  

Other supported collection types in the `codecs` package:

```scala mdoc
import zio.NonEmptyChunk
import zio.prelude.NonEmptyList
import zio.prelude.ZSet

val arr = serializeToArray(Array(1, 2, 3, 4))
val set = serializeToArray(Set(1, 2, 3, 4))
val sortedSet = serializeToArray(SortedSet(1, 2, 3, 4))

val nec = serializeToArray(NonEmptyChunk(1, 2, 3, 4))
val nel = serializeToArray(NonEmptyList(1, 2, 3, 4))
val nes = serializeToArray(ZSet(1, 2, 3, 4))
```

### String deduplication
For strings the library have a simple deduplication system, without sacrificing any extra
bytes for cases when strings are not duplicate. In general, the strings are encoded by a variable length
int representing the length of the string in bytes, followed by its UTF-8 encoding. 
When deduplication is enabled, each serialized 
string gets an ID and if it is serialized once more in the same stream, a negative number in place of the 
length identifies it.   

```scala mdoc
val twoStrings1 = serializeToArray(List("Hello", "Hello"))

val twoStrings2 = serializeToArray(List(DeduplicatedString("Hello"), DeduplicatedString("Hello")))
```

It is not turned on by default because it breaks backward compatibility when evolving data structures.
If a new string field is added, old versions of the application will skip it and would not assign the
same ID to the string if it is first seen. 

It is enabled internally in desert for some cases, and can be used in _custom serializers_ freely. 

### Tuples
The elements of tuples are serialized flat and the whole tuple gets prefixed by `0`, which makes them
compatible with simple _case classes_:

```scala mdoc
val tup = serializeToArray((1, 2, 3)) 
```

### Maps
`Map`, `SortedMap` and `NonEmptyMap` are just another `iterableCodec` built on top of the _tuple support_
for serializing an iteration of key-value pairs:

```scala mdoc
import scala.collection.immutable.SortedMap

val map = serializeToArray(Map(1 -> "x", 2 -> "y"))
val sortedmap = serializeToArray(SortedMap(1 -> "x", 2 -> "y"))
```

### Generic codecs for ADTs
There is a generic derivable codec for algebraic data types, with support for [evolving the type](evolution)
during the lifecycle of the application.

For _case classes_ the representation is the same as for tuples:

```scala mdoc
case class Point(x: Int, y: Int, z: Int)
object Point {
  implicit val codec: BinaryCodec[Point] = DerivedBinaryCodec.derive
}

val pt = serializeToArray(Point(1, 2, 3))
```

Note the empty parameter list for `BinaryCodec.derive`. It is where the **evolution steps** are defined, 
explained on a [separate section](evolution). When it is empty the only additional storage cost is the
single `0` byte on the beginning, just like with tuples.

For _sum types_ the codec is not automatically derived for all the constructors. This is by design, as the
evolution steps has to be specified one by one per constructor. Other than that it works the same way, 
with `derive`:

```scala mdoc
sealed trait Drink
case class Beer(typ: String) extends Drink
case object Water extends Drink

object Drink {
  implicit val beerCodec: BinaryCodec[Beer] = DerivedBinaryCodec.derive
  implicit val waterCodec: BinaryCodec[Water.type] = DerivedBinaryCodec.derive
  implicit val codec: BinaryCodec[Drink] = DerivedBinaryCodec.derive
}

val a = serializeToArray[Drink](Beer("X"))
val b = serializeToArray[Drink](Water)
```

### Transient fields in generic codecs
It is possible to mark some fields of a _case class_ as **transient**:

```scala mdoc
case class Point2(x: Int, y: Int, z: Int, @transientField(None) cachedDistance: Option[Double])
object Point2 {
  implicit val codec: BinaryCodec[Point2] = DerivedBinaryCodec.derive
}

val pt2 = for {
  data <- serializeToArray(Point2(1, 2, 3, Some(3.7416)))
  result <- deserializeFromArray[Point2](data)
} yield result
```

Transient fields are not being serialized and they get a default value contained by the annotation
during deserialization. Note that the default value is not type checked during compilation, if
it does not match the field type it causes runtime error. 

### Transient constructors in generic codecs
It is possible to mark whole constructors as **transient**:

```scala mdoc
sealed trait Cases
@transientConstructor case class Case1() extends Cases
case class Case2() extends Cases

object Cases {
  implicit val case2Codec: BinaryCodec[Case2] = DerivedBinaryCodec.derive
  implicit val codec: BinaryCodec[Cases] = DerivedBinaryCodec.derive
}

val cs1 = serializeToArray[Cases](Case1())
val cs2 = serializeToArray[Cases](Case2())
```

Transient constructors cannot be serialized. A common use case is for remote accessible actors where 
some actor messages are known to be local only. By marking them as transient they can hold non-serializable data
without breaking the serialization of the other, remote messages.

### Generic codecs for value type wrappers
It is a good practice to use zero-cost value type wrappers around primitive types to represent
the intention in the type system. `desert` can derive binary codecs for these too:

```scala mdoc
case class DocumentId(id: Long) // extends AnyVal
object DocumentId {
  implicit val codec: BinaryCodec[DocumentId] = DerivedBinaryCodec.deriveForWrapper
}

val id = serializeToArray(DocumentId(100))
``` 

### Custom codecs
The _serialization_ is a monadic function:

```scala
def serialize(value: T): Ser[Unit]
```

while the _deserialization_ is
```scala
def deserialize(): Deser[T]
```

where 

```scala
type Ser[T] = ZPure[Nothing, SerializerState, SerializerState, SerializationEnv, DesertFailure, T]
type Deser[T] = ZPure[Nothing, SerializerState, SerializerState, DeserializationEnv, DesertFailure, T]
```

With the `BinaryCodec.define` function it is possible to define a fully custom codec. In the following
example we define a data type capable of representing cyclic graphs via a mutable `next` field, and 
a custom codec for deserializing it. It also shows that built-in support for tracking _object references_
which is not used by the generic codecs but can be used in scenarios like this.

```scala mdoc
import cats.instances.either._

  class Node(val label: String,
             var next: Option[Node]) {
    override def toString: String = 
      next match {
       case Some(n) => s"<$label -> ${n.label}>"
       case None => s"<$label>"
      }
  }
  object Node {
    implicit def codec: BinaryCodec[Node] = BinaryCodec.define[Node](
      // Serializer function
      node => for {
        _ <- write(node.label) // write the label using the built-in string codec
        _ <- node.next match {
          case Some(value) =>
            for {
              _ <- write(true) // next is defined (built-in boolean codec)
              _ <- storeRefOrObject(value) // store ref-id or serialize next
            } yield ()
          case None =>
            write(false) // next is undefined (built-in boolean codec)
        }
      } yield ()
    )(for { // Deserializer function
      label <- read[String]()        // read the label using the built-in string codec
      result = new Node(label, None) // create the new node
      _ <- storeReadRef(result)      // store the node in the reference map
      hasNext <- read[Boolean]()     // read if 'next' is defined
      _ <- if (hasNext ) {
        // Read next with reference-id support and mutate the result
        readRefOrValue[Node](storeReadReference = false).map { value => result.next = Some(value) }
      } else finishDeserializerWith(())
    } yield result)
  }

  case class Root(node: Node)
  object Root {
    implicit val codec: BinaryCodec[Root] = BinaryCodec.define[Root](
      root => storeRefOrObject(root.node)
    )(readRefOrValue[Node](storeReadReference = false).map(Root.apply))
  }

val nodeA = new Node("a", None)
val nodeB = new Node("a", None)
val nodeC = new Node("a", None)
nodeA.next = Some(nodeB)
nodeB.next = Some(nodeC)
nodeC.next = Some(nodeA)

val result = serializeToArray(Root(nodeA))
``` 
