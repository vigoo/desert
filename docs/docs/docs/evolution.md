---
layout: docs
title: Evolution
---

# Evolution
One of the primary features of `desert` is to support data type evolution via its generic codec for 
ADTs, and with some other design decisions.

In this section we review what kind of changes can be done to the data model without breaking binary 
compatibility.

### Tuples vs products
As already mentioned on the [codecs page](codecs), tuples and non-evolved case classes are binary
compatible. This means that it is possible to convert a tuple to a case class without breaking
the serialization format:

```scala mdoc
import io.github.vigoo.desert._
import io.github.vigoo.desert.Evolution._
import io.github.vigoo.desert.shapeless._

case class Point(x: Int, y: Int)
object Point {
  implicit val codec: BinaryCodec[Point] = DerivedBinaryCodec.derive
}

val ex1 = for {
  data1 <- serializeToArray((5, 6))
  pt1 <- deserializeFromArray[Point](data1)
  data2 <- serializeToArray(pt1)
  tup2 <- deserializeFromArray[(Int, Int)](data2)
} yield (pt1, tup2)
```

### Primitives vs wrappers
Codecs derived with `deriveForWrapper` are also fully compatible with their underlying primitive
type, so it is fully safe to evolve the date model to make use of more and more value type wrappers:

```scala mdoc
case class Id(id: Int) // extends AnyVal
object Id {
  implicit val codec: BinaryCodec[Id] = DerivedBinaryCodec.deriveForWrapper
}

val ex2 = for {
  data1 <- serializeToArray(3)
  id1 <- deserializeFromArray[Id](data1)
  data2 <- serializeToArray(id1)
  raw1 <- deserializeFromArray[Int](data2)
} yield (id1, raw1)
```

### Collections
Collection codecs share their binary representation via `iterableCodec` and they can be freely
replaced:

```scala mdoc
val ex3 = for {
  data1 <- serializeToArray(List(1, 2, 3))
  set1 <- deserializeFromArray[Set[Int]](data1)
  data2 <- serializeToArray(set1)
  vec1 <- deserializeFromArray[Vector[Int]](data2)
} yield (set1, vec1)
``` 

### Adding a new field
If a _case class_ gets extended by a new field, it has to be explicitly marked with an **evolution step**
passed as a parameter to the `derive` function. For addig a new field, this defines a _default value_ to be
used when deserializing an old data. With this the old and the new data model remains fully compatible:

- If the old version reads a new data, it skips the new field
- If the new version reads an old data, it uses the provided default to set the field

```scala mdoc
case class PointV1(x: Int, y: Int)
object PointV1 {
  implicit val codec: BinaryCodec[PointV1] = DerivedBinaryCodec.derive
}

@evolutionSteps(FieldAdded[Int]("z", 1))
case class PointV2(x: Int, y: Int, z: Int)
object PointV2 {
  implicit val codec: BinaryCodec[PointV2] = DerivedBinaryCodec.derive
}

val ex4 = for {
  oldData <- serializeToArray(PointV1(10, 20))
  newPt <- deserializeFromArray[PointV2](oldData)
  newData <- serializeToArray(newPt)
  oldPt <- deserializeFromArray[PointV1](newData)
} yield (oldPt, newPt)
``` 

Note that the added field does not have to be last position of the case class. 

### Making a field optional
Another supported evolution step is _making a field optional_. This can be an intermediate step
before completely removing an obsolete field. Once again this change has to be recorded in the parameter list for `derive`. With that, we get 
the following capabilities:

- If the old version reads the new data, and it is `Some(x)`, it will be read as `x`
- If the old version reads the new data, and it is `None` the serialization fails
- If the new version reads the old data, it automatically wraps the field in `Some`

```scala mdoc
@evolutionSteps(
  FieldAdded[Int]("z", 1),
  FieldMadeOptional("z")
)
case class PointV3(x: Int, y: Int, z: Option[Int])
object PointV3 {
  implicit val codec: BinaryCodec[PointV3] = DerivedBinaryCodec.derive
}

val ex5 = for {
  v1Data <- serializeToArray(PointV1(10, 20))
  newPt <- deserializeFromArray[PointV3](v1Data)
  newData <- serializeToArray(newPt)
  v2Pt <- deserializeFromArray[PointV2](newData)
} yield (v2Pt, newPt)

val ex5fail = for {
  v3Data <- serializeToArray(PointV3(10, 20, None))
  v2Pt <- deserializeFromArray[PointV2](v3Data)
} yield v2Pt
```

Note that any field can be made optional, not just the one which was added later like in this example.

### Removing a field
The third supported evolution step is _removing a field_. Here backward compatibility has a limitation:

- New version can read old data by simply skipping the removed field
- Old version can only read new data if the removed field was an `Option[T]`, reading it as `None`

```scala mdoc
@evolutionSteps(
  FieldAdded[Int]("z", 1),
  FieldMadeOptional("z"),
  FieldRemoved("z")
)
case class PointV4(x: Int, y: Int)
object PointV4 {
  implicit val codec: BinaryCodec[PointV4] = DerivedBinaryCodec.derive
}

val ex6 = for {
  v2Data <- serializeToArray(PointV2(10, 20, 30))
  newPt <- deserializeFromArray[PointV4](v2Data)
  newData <- serializeToArray(newPt)
  v3Pt <- deserializeFromArray[PointV3](newData)
} yield (v3Pt, newPt)

val ex6fail = for {
  v4Data <- serializeToArray(PointV4(10, 20))
  v2Pt <- deserializeFromArray[PointV2](v4Data)
} yield v2Pt
```

### Transient fields
Adding a new transient field does not change the binary representation.

Making an existing field transient can be recorded as an _evolution step_ called `FieldMadeTransient`.
It is just an alias for `FieldRemoved` from the serializer's point of view, so the same rules apply
as for field removal.

```scala mdoc
@evolutionSteps(
  FieldAdded[Int]("z", 1),
  FieldMadeOptional("z"),
  FieldRemoved("z"),
  FieldMadeTransient("y")
)
case class PointV5(x: Int, @transientField(0) y: Int)
object PointV5 {
  implicit val codec: BinaryCodec[PointV5] = DerivedBinaryCodec.derive
}

val ex7 = for {
  v4Data <- serializeToArray(PointV4(10, 20))
  newPt <- deserializeFromArray[PointV5](v4Data)
} yield newPt

val ex7fail = for {
  v5Data <- serializeToArray(PointV5(10, 20))
  v4Pt <- deserializeFromArray[PointV4](v5Data)
} yield v4Pt
```

### Adding a new constructor
Adding a new constructor to a _sealed trait_ is allowed, but it has to be added to the end of the 
list to maintain _constructor ID order_. For the same reason it is currently not supported to remove
a constructor either. Each constructor can be evolved separately with the above methods as they have 
their own evolution steps.  

### Adding a new transient constructor
Constructors marked with `transientConstructor` are not getting an associated _constructor ID_ so they can
be inserted or get removed freely.

### Type registry placeholders
When using _type identifiers_ with the [type registry](type-registry) and a previously registered
type has been removed from the code, it's place can be kept assigned by using `registerPlaceholder`
in the registration:

```scala mdoc:invisible
case class Impl2()
object Impl2 { implicit val codec: BinaryCodec[Impl2] = DerivedBinaryCodec.derive }
```

```scala mdoc:silent
val typeRegistry2 = DefaultTypeRegistry()
  .registerPlaceholder() // previously: .register[Impl1]
  .register[Impl2]
  .freeze()
```

### Generic product evolution encoding
This section gives an overview of how the above described evolution steps are being encoded in the 
binary representation of case classes.

Let's examine the output of serializing the above examples!

```scala mdoc:serialized
val v1 = serializeToArray(PointV1(100, 200))
```

- The first byte is the **version** and it is `0` because there are no evolution steps for this type.
- This makes it compatible with a `(Int, Int)` pair
- `0, 0, 0, 100` is the fixed 32-bit integer representation of `100`
- `0, 0, 0, -56` is the fixed 32-bit integer representation of `200`
- So `PointV1` is always serialized into _9 bytes_

```scala mdoc:serialized
val v2 = serializeToArray(PointV2(100, 200, 300))
```

- The first byte is **version** (or in other words, the number of evolution steps) which is now `1`
- Because the version is non-zero, the next item to read is a _variable-width integer_ representing the chunk size of the original version-0 data
- In this case it takes 1 byte: `16` which represents `8`, the sum of the two 32 bit fields `x` and `y`
- Following it the next item is again a _variable-width integer_ representing the chunk size for the single newly added field `z`
- It is again 1 byte: `8` which means `4`
- The rest is the first chunk containing the fields from _V1_ and then the next chunk containing the field from _V2_
- The old version can use the chunk size data to skip the unknown fields and only read the first one
- `PointV2` takes _15 bytes_ by having 3 bytes of header and 12 bytes of data

```scala mdoc:serialized
val v3 = serializeToArray(PointV3(100, 200, Some(300)))
```

- The first byte is now `2` as we added a new evolution step
- The header part starts with almost the same two values, encoded as `16, 10`
- The first chunk is still 8 byte long, but the next one is now `10` which means `5`, because the `Option` wrapper adds an extra byte to differentiate `None` or `Some`
- After the two variable integers we have a third one corresponding to the third evolution step. But as it did not add any new fields, it is not a chunk size, but a special 
negative number marking the field as optional.
- This is the value `1` which in fact represents `-1` in the variable-length integer encoding. This code is
always followed by a serialized _field position_, which is again a variable-length integer, in this case `1` aka `-1` again.
  - Negative field positions address a non-first chunk
  - Positive field positions address a field in the first chunk
  - This is for space efficiency. The first chunk is kept flat without any chunk size info needed so non-evolved data type serialization is almost zero cost.
- The rest is the data itself, first chunk is still _8 bytes_
- Second chunk is now _5 bytes_, the `1` indicates that it is `Some` and the rest 4 bytes are the 32-bit integer
- `PointV3` takes _18 bytes_ in total, 5 bytes of header and 13 bytes of data
   
```scala mdoc:serialized
val v4 = serializeToArray(PointV4(100, 200))
```

- The first byte is now `3`
- The first chunk size is still `16` because we removed the field from the second chunk
- The next integer still represents the chunk size for the second evolution step, which was adding the field _z_. But as it has been later removed, the chunk size is now `0`
- The third variable integer in the header part is `1` representing that a field has been removed.
- It is followed by the _field position_ which in `V3` was `-1` pointing to the second chunk. But as that is removed now, it is a special value `-128`, which is `FieldPosition.removed`
- The fourth variable integer is again a special code, encoded as `3` representing the value `-2`. This is the code for field removal.
- It is followed by a _serialized string_ containing the removed field's name. String deduplication is enabled here as it was described in the [codecs page](codecs).
- As this is the first occurrence of the string `"z"` it is encoded as `2, 122`. 
- The rest 8 bytes are the first chunk containing `x` and `y`
- So `PointV4` in this example takes _16 bytes_ in total, where 8 bytes are header and 8 bytes are data. The exact size depends on the string IDs so it can change in real world situations.

