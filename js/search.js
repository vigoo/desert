// When the user clicks on the search box, we want to toggle the search dropdown
function displayToggleSearch(e) {
  e.preventDefault();
  e.stopPropagation();

  closeDropdownSearch(e);
  
  if (idx === null) {
    console.log("Building search index...");
    prepareIdxAndDocMap();
    console.log("Search index built.");
  }
  const dropdown = document.querySelector("#search-dropdown-content");
  if (dropdown) {
    if (!dropdown.classList.contains("show")) {
      dropdown.classList.add("show");
    }
    document.addEventListener("click", closeDropdownSearch);
    document.addEventListener("keydown", searchOnKeyDown);
    document.addEventListener("keyup", searchOnKeyUp);
  }
}

//We want to prepare the index only after clicking the search bar
var idx = null
const docMap = new Map()

function prepareIdxAndDocMap() {
  const docs = [  
    {
      "title": "Akka",
      "url": "/desert/docs/akka/",
      "content": "Akka integration The akka module adds some extra codecs and an akka serializer. To use it add the dependency: libraryDependencies += \"io.github.vigoo\" %% \"desert-akka\" % \"0.2.0\" Codecs The following codecs are added: Codec for Akka Classic actor references (BinaryCodec[ActorRef]) Codec for Akka typed actor references (BinaryCodec[ActorRef[T]]) Codec for ByteString Syntax Some akka-specific syntax has been added to io.github.vigoo.desert.akka.syntax._: serializeToByteString serializeUnknownToByteString deserializeFromByteString deserializeUnknownFromByteString Akka Serialization desert can be used as an Akka serializer for remoting and persistence by inheriting from DesertSerializerBase and setting up a type registry in it. import io.github.vigoo.desert._ import io.github.vigoo.desert.codecs._ import io.github.vigoo.desert.akka._ class DesertSerializer extends DesertSerializerBase { override val typeRegistry: TypeRegistry = DefaultTypeRegistry() // .register[X]() // ... // .register[Z]() .freeze() }"
    } ,    
    {
      "title": "Cats Effect",
      "url": "/desert/docs/cats-effect/",
      "content": "Cats Effect integration The cats-effect module simply wraps the top-level serialization functions into effects. To use it add the dependency: libraryDependencies += \"io.github.vigoo\" %% \"desert-cats-effect\" % \"0.2.0\" then import import io.github.vigoo.desert._ import io.github.vigoo.desert.codecs._ import io.github.vigoo.desert.catseffect.syntax._ to get variants like def serializeToArray[F[_] : Sync, T: BinarySerializer](value: T, typeRegistry: TypeRegistry = TypeRegistry.empty): F[Array[Byte]]"
    } ,    
    {
      "title": "Cats Effect",
      "url": "/desert/docs/cats-effect/",
      "content": "Cats integration The cats module contains some additional codecs for cats data types. To use it add the dependency: libraryDependencies += \"io.github.vigoo\" %% \"desert-cats\" % \"0.2.0\" then import import cats.data._ import io.github.vigoo.desert._ import io.github.vigoo.desert.codecs._ import io.github.vigoo.desert.cats.codecs._ to get codecs for: val valid = serializeToArray[Validated[String, Int]](Validated.Valid(100)) val invalid = serializeToArray[Validated[String, Int]](Validated.Invalid(\"error\")) val nel = serializeToArray(NonEmptyList.of(1, 2, 3, 4)) val nes = serializeToArray(NonEmptySet.of(1, 2, 3, 4)) val nem = serializeToArray(NonEmptyMap.of(1 -&gt; \"x\", 2 -&gt; \"y\"))"
    } ,    
    {
      "title": "Codecs",
      "url": "/desert/docs/codecs/",
      "content": "Codecs A BinaryCodec[T] defines both the serializer and deserializer for a given type: trait BinaryCodec[T] extends BinarySerializer[T] with BinaryDeserializer[T] Primitive types The io.github.vigoo.desert.codecs module defines a lot of implicit binary codecs for common types. The following code examples demonstrate this and also shows how the binary representation looks like. import io.github.vigoo.desert.{BinaryCodec, TransientConstructor, TransientField} import io.github.vigoo.desert.codecs._ import io.github.vigoo.desert.syntax._ val byte = serializeToArray(100.toByte) // byte: Either[io.github.vigoo.desert.DesertFailure, Array[Byte]] = Right( // value = Array(100) // ) val short = serializeToArray(100.toShort) // short: Either[io.github.vigoo.desert.DesertFailure, Array[Byte]] = Right( // value = Array(0, 100) // ) val int = serializeToArray(100) // int: Either[io.github.vigoo.desert.DesertFailure, Array[Byte]] = Right( // value = Array(0, 0, 0, 100) // ) val long = serializeToArray(100L) // long: Either[io.github.vigoo.desert.DesertFailure, Array[Byte]] = Right( // value = Array(0, 0, 0, 0, 0, 0, 0, 100) // ) val float = serializeToArray(3.14.toFloat) // float: Either[io.github.vigoo.desert.DesertFailure, Array[Byte]] = Right( // value = Array(64, 72, -11, -61) // ) val double = serializeToArray(3.14) // double: Either[io.github.vigoo.desert.DesertFailure, Array[Byte]] = Right( // value = Array(64, 9, 30, -72, 81, -21, -123, 31) // ) val bool = serializeToArray(true) // bool: Either[io.github.vigoo.desert.DesertFailure, Array[Byte]] = Right( // value = Array(1) // ) val unit = serializeToArray(()) // unit: Either[io.github.vigoo.desert.DesertFailure, Array[Byte]] = Right( // value = Array() // ) val str = serializeToArray(\"Hello\") // str: Either[io.github.vigoo.desert.DesertFailure, Array[Byte]] = Right( // value = Array(10, 72, 101, 108, 108, 111) // ) val uuid = serializeToArray(java.util.UUID.randomUUID()) // uuid: Either[io.github.vigoo.desert.DesertFailure, Array[Byte]] = Right( // value = Array( // -80, // 56, // -15, // -40, // 31, // 113, // 79, // 102, // -77, // 117, // -120, // -57, // 55, // -37, // -73, // 30 // ) // ) Option, Either, Try, Validation Common types such as Option and Either are also supported out of the box. For Try it also has a codec for arbitrary Throwable instances, although deserializing it does not recreate the original throwable just a PersistedThrowable instance. In practice this is a much safer approach than trying to recreate the same exception via reflection. import scala.collection.immutable.SortedSet import scala.util._ import zio.NonEmptyChunk import zio.prelude.Validation val none = serializeToArray[Option[Int]](None) // none: Either[io.github.vigoo.desert.DesertFailure, Array[Byte]] = Right( // value = Array(0) // ) val some = serializeToArray[Option[Int]](Some(100)) // some: Either[io.github.vigoo.desert.DesertFailure, Array[Byte]] = Right( // value = Array(1, 0, 0, 0, 100) // ) val left = serializeToArray[Either[Boolean, Int]](Left(true)) // left: Either[io.github.vigoo.desert.DesertFailure, Array[Byte]] = Right( // value = Array(0, 1) // ) val right = serializeToArray[Either[Boolean, Int]](Right(100)) // right: Either[io.github.vigoo.desert.DesertFailure, Array[Byte]] = Right( // value = Array(1, 0, 0, 0, 100) // ) val valid = serializeToArray[Validation[String, Int]](Validation.Success(100)) // valid: Either[io.github.vigoo.desert.DesertFailure, Array[Byte]] = Right( // value = Array(1, 0, 0, 0, 100) // ) val invalid = serializeToArray[Validation[String, Int]](Validation.Failure(NonEmptyChunk(\"error\"))) // invalid: Either[io.github.vigoo.desert.DesertFailure, Array[Byte]] = Right( // value = Array(0, 2, 10, 101, 114, 114, 111, 114) // ) val fail = serializeToArray[Try[Int]](Failure(new RuntimeException(\"Test exception\"))) val failDeser = fail.flatMap(data =&gt; deserializeFromArray[Try[Int]](data)) // failDeser: Either[io.github.vigoo.desert.DesertFailure, Try[Int]] = Right( // value = Failure( // exception = PersistedThrowable( // className = \"java.lang.RuntimeException\", // message = \"Test exception\", // stackTrace = Array( // repl.MdocSession$App.&lt;init&gt;(codecs.md:83), // repl.MdocSession$.app(codecs.md:3), // mdoc.internal.document.DocumentBuilder$$doc$.$anonfun$build$2(DocumentBuilder.scala:89), // scala.runtime.java8.JFunction0$mcV$sp.apply(JFunction0$mcV$sp.scala:18), // scala.util.DynamicVariable.withValue(DynamicVariable.scala:59), // scala.Console$.withErr(Console.scala:193), // mdoc.internal.document.DocumentBuilder$$doc$.$anonfun$build$1(DocumentBuilder.scala:89), // scala.runtime.java8.JFunction0$mcV$sp.apply(JFunction0$mcV$sp.scala:18), // scala.util.DynamicVariable.withValue(DynamicVariable.scala:59), // scala.Console$.withOut(Console.scala:164), // mdoc.internal.document.DocumentBuilder$$doc$.build(DocumentBuilder.scala:88), // mdoc.internal.markdown.MarkdownBuilder$.$anonfun$buildDocument$2(MarkdownBuilder.scala:47), // mdoc.internal.markdown.MarkdownBuilder$$anon$1.run(MarkdownBuilder.scala:103) // ), // cause = None // ) // ) // ) val success = serializeToArray[Try[Int]](Success(100)) // success: Either[io.github.vigoo.desert.DesertFailure, Array[Byte]] = Right( // value = Array(1, 0, 0, 0, 100) // ) Collections There is a generic iterableCodec that can be used to define implicit collection codecs based on the Scala 2.13 collection API. For example this is how the vectorCodec is defined: implicit def vectorCodec[A : BinaryCodec]: BinaryCodec[Vector[A]] = iterableCodec[A, Vector[A]] All these collection codecs have one of the two possible representation. If the size is known in advance then it is the number of elements followed by all the items in iteration order, otherwise it is a flat list of all the elements wrapped in Option[T]. Vector and List are good examples for the two: val vec = serializeToArray(Vector(1, 2, 3, 4)) // vec: Either[io.github.vigoo.desert.DesertFailure, Array[Byte]] = Right( // value = Array(8, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4) // ) val lst = serializeToArray(List(1, 2, 3, 4)) // lst: Either[io.github.vigoo.desert.DesertFailure, Array[Byte]] = Right( // value = Array( // 1, // 1, // 0, // 0, // 0, // 1, // 1, // 0, // 0, // 0, // 2, // 1, // 0, // 0, // 0, // 3, // 1, // 0, // 0, // 0, // 4, // 0 // ) // ) Other supported collection types in the codecs package: import zio.NonEmptyChunk import zio.prelude.NonEmptyList import zio.prelude.ZSet val arr = serializeToArray(Array(1, 2, 3, 4)) // arr: Either[io.github.vigoo.desert.DesertFailure, Array[Byte]] = Right( // value = Array(8, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4) // ) val set = serializeToArray(Set(1, 2, 3, 4)) // set: Either[io.github.vigoo.desert.DesertFailure, Array[Byte]] = Right( // value = Array(8, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4) // ) val sortedSet = serializeToArray(SortedSet(1, 2, 3, 4)) // sortedSet: Either[io.github.vigoo.desert.DesertFailure, Array[Byte]] = Right( // value = Array( // 1, // 1, // 0, // 0, // 0, // 1, // 1, // 0, // 0, // 0, // 2, // 1, // 0, // 0, // 0, // 3, // 1, // 0, // 0, // 0, // 4, // 0 // ) // ) val nec = serializeToArray(NonEmptyChunk(1, 2, 3, 4)) // nec: Either[io.github.vigoo.desert.DesertFailure, Array[Byte]] = Right( // value = Array(8, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4) // ) val nel = serializeToArray(NonEmptyList(1, 2, 3, 4)) // nel: Either[io.github.vigoo.desert.DesertFailure, Array[Byte]] = Right( // value = Array( // 1, // 1, // 0, // 0, // 0, // 1, // 1, // 0, // 0, // 0, // 2, // 1, // 0, // 0, // 0, // 3, // 1, // 0, // 0, // 0, // 4, // 0 // ) // ) val nes = serializeToArray(ZSet(1, 2, 3, 4)) // nes: Either[io.github.vigoo.desert.DesertFailure, Array[Byte]] = Right( // value = Array( // 8, // 0, // 0, // 0, // 0, // 1, // 0, // 0, // 0, // 1, // 0, // 0, // 0, // 0, // 2, // 0, // 0, // 0, // 1, // 0, // 0, // 0, // 0, // 3, // 0, // 0, // 0, // 1, // 0, // 0, // 0, // 0, // 4, // 0, // 0, // 0, // 1 // ) // ) String deduplication For strings the library have a simple deduplication system, without sacrificing any extra bytes for cases when strings are not duplicate. In general, the strings are encoded by a variable length int representing the length of the string in bytes, followed by its UTF-8 encoding. When deduplication is enabled, each serialized string gets an ID and if it is serialized once more in the same stream, a negative number in place of the length identifies it. val twoStrings1 = serializeToArray(List(\"Hello\", \"Hello\")) // twoStrings1: Either[io.github.vigoo.desert.DesertFailure, Array[Byte]] = Right( // value = Array( // 1, // 1, // 10, // 72, // 101, // 108, // 108, // 111, // 1, // 10, // 72, // 101, // 108, // 108, // 111, // 0 // ) // ) val twoStrings2 = serializeToArray(List(DeduplicatedString(\"Hello\"), DeduplicatedString(\"Hello\"))) // twoStrings2: Either[io.github.vigoo.desert.DesertFailure, Array[Byte]] = Right( // value = Array(1, 1, 10, 72, 101, 108, 108, 111, 1, 1, 0) // ) It is not turned on by default because it breaks backward compatibility when evolving data structures. If a new string field is added, old versions of the application will skip it and would not assign the same ID to the string if it is first seen. It is enabled internally in desert for some cases, and can be used in custom serializers freely. Tuples The elements of tuples are serialized flat and the whole tuple gets prefixed by 0, which makes them compatible with simple case classes: val tup = serializeToArray((1, 2, 3)) // tup: Either[io.github.vigoo.desert.DesertFailure, Array[Byte]] = Right( // value = Array(0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3) // ) Maps Map, SortedMap and NonEmptyMap are just another iterableCodec built on top of the tuple support for serializing an iteration of key-value pairs: import scala.collection.immutable.SortedMap val map = serializeToArray(Map(1 -&gt; \"x\", 2 -&gt; \"y\")) // map: Either[io.github.vigoo.desert.DesertFailure, Array[Byte]] = Right( // value = Array(4, 0, 0, 0, 0, 1, 2, 120, 0, 0, 0, 0, 2, 2, 121) // ) val sortedmap = serializeToArray(SortedMap(1 -&gt; \"x\", 2 -&gt; \"y\")) // sortedmap: Either[io.github.vigoo.desert.DesertFailure, Array[Byte]] = Right( // value = Array(4, 0, 0, 0, 0, 1, 2, 120, 0, 0, 0, 0, 2, 2, 121) // ) Generic codecs for ADTs There is a generic derivable codec for algebraic data types, with support for evolving the type during the lifecycle of the application. For case classes the representation is the same as for tuples: case class Point(x: Int, y: Int, z: Int) object Point { implicit val codec: BinaryCodec[Point] = BinaryCodec.derive() } val pt = serializeToArray(Point(1, 2, 3)) // pt: Either[io.github.vigoo.desert.DesertFailure, Array[Byte]] = Right( // value = Array(0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3) // ) Note the empty parameter list for BinaryCodec.derive. It is where the evolution steps are defined, explained on a separate section. When it is empty the only additional storage cost is the single 0 byte on the beginning, just like with tuples. For sum types the codec is not automatically derived for all the constructors. This is by design, as the evolution steps has to be specified one by one per constructor. Other than that it works the same way, with derive: sealed trait Drink case class Beer(typ: String) extends Drink case object Water extends Drink object Drink { implicit val beerCodec: BinaryCodec[Beer] = BinaryCodec.derive() implicit val waterCodec: BinaryCodec[Water.type] = BinaryCodec.derive() implicit val codec: BinaryCodec[Drink] = BinaryCodec.derive() } val a = serializeToArray[Drink](Beer(\"X\")) // a: Either[io.github.vigoo.desert.DesertFailure, Array[Byte]] = Right( // value = Array(0, 0, 0, 2, 88) // ) val b = serializeToArray[Drink](Water) // b: Either[io.github.vigoo.desert.DesertFailure, Array[Byte]] = Right( // value = Array(0, 1, 0) // ) Transient fields in generic codecs It is possible to mark some fields of a case class as transient: case class Point2(x: Int, y: Int, z: Int, @TransientField(None) cachedDistance: Option[Double]) object Point2 { implicit val codec: BinaryCodec[Point2] = BinaryCodec.derive() } val pt2 = for { data &lt;- serializeToArray(Point2(1, 2, 3, Some(3.7416))) result &lt;- deserializeFromArray[Point2](data) } yield result // pt2: Either[io.github.vigoo.desert.DesertFailure, Point2] = Right( // value = Point2(x = 1, y = 2, z = 3, cachedDistance = None) // ) Transient fields are not being serialized and they get a default value contained by the annotation during deserialization. Note that the default value is not type checked during compilation, if it does not match the field type it causes runtime error. Transient constructors in generic codecs It is possible to mark whole constructors as transient: sealed trait Cases @TransientConstructor case class Case1() extends Cases case class Case2() extends Cases object Cases { implicit val case2Codec: BinaryCodec[Case2] = BinaryCodec.derive() implicit val codec: BinaryCodec[Cases] = BinaryCodec.derive() } val cs1 = serializeToArray[Cases](Case1()) // cs1: Either[io.github.vigoo.desert.DesertFailure, Array[Byte]] = Left( // value = SerializingTransientConstructor(name = \"Case1\") // ) val cs2 = serializeToArray[Cases](Case2()) // cs2: Either[io.github.vigoo.desert.DesertFailure, Array[Byte]] = Right( // value = Array(0, 0, 0) // ) Transient constructors cannot be serialized. A common use case is for remote accessible actors where some actor messages are known to be local only. By marking them as transient they can hold non-serializable data without breaking the serialization of the other, remote messages. Generic codecs for value type wrappers It is a good practice to use zero-cost value type wrappers around primitive types to represent the intention in the type system. desert can derive binary codecs for these too: case class DocumentId(id: Long) // extends AnyVal // extends AnyVal object DocumentId { implicit val codec: BinaryCodec[DocumentId] = BinaryCodec.deriveForWrapper } val id = serializeToArray(DocumentId(100)) // id: Either[io.github.vigoo.desert.DesertFailure, Array[Byte]] = Right( // value = Array(0, 0, 0, 0, 0, 0, 0, 100) // ) Custom codecs The serialization is a monadic function: def serialize(value: T): Ser[Unit] while the deserialization is def deserialize(): Deser[T] where type Ser[T] = ReaderT[StateT[EitherT[Eval, DesertFailure, *], SerializerState, *], SerializationEnv, T] type Deser[T] = ReaderT[StateT[EitherT[Eval, DesertFailure, *], SerializerState, *], DeserializationEnv, T] With the BinaryCodec.define function it is possible to define a fully custom codec. In the following example we define a data type capable of representing cyclic graphs via a mutable next field, and a custom codec for deserializing it. It also shows that built-in support for tracking object references which is not used by the generic codecs but can be used in scenarios like this. import cats.instances.either._ class Node(val label: String, var next: Option[Node]) { override def toString: String = next match { case Some(n) =&gt; s\"&lt;$label -&gt; ${n.label}&gt;\" case None =&gt; s\"&lt;$label&gt;\" } } object Node { implicit def codec: BinaryCodec[Node] = BinaryCodec.define[Node]( // Serializer function node =&gt; for { _ &lt;- write(node.label) // write the label using the built-in string codec _ &lt;- node.next match { case Some(value) =&gt; for { _ &lt;- write(true) // next is defined (built-in boolean codec) _ &lt;- storeRefOrObject(value) // store ref-id or serialize next } yield () case None =&gt; write(false) // next is undefined (built-in boolean codec) } } yield () )(for { // Deserializer function label &lt;- read[String]() // read the label using the built-in string codec result = new Node(label, None) // create the new node _ &lt;- storeReadRef(result) // store the node in the reference map hasNext &lt;- read[Boolean]() // read if 'next' is defined _ &lt;- if (hasNext ) { // Read next with reference-id support and mutate the result readRefOrValue[Node](storeReadReference = false).map { value =&gt; result.next = Some(value) } } else finishDeserializerWith(()) } yield result) } case class Root(node: Node) object Root { implicit val codec: BinaryCodec[Root] = BinaryCodec.define[Root]( root =&gt; storeRefOrObject(root.node) )(readRefOrValue[Node](storeReadReference = false).map(Root.apply)) } val nodeA = new Node(\"a\", None) // nodeA: Node = &lt;a -&gt; a&gt; val nodeB = new Node(\"a\", None) // nodeB: Node = &lt;a -&gt; a&gt; val nodeC = new Node(\"a\", None) // nodeC: Node = &lt;a -&gt; a&gt; nodeA.next = Some(nodeB) nodeB.next = Some(nodeC) nodeC.next = Some(nodeA) val result = serializeToArray(Root(nodeA)) // result: Either[io.github.vigoo.desert.DesertFailure, Array[Byte]] = Right( // value = Array(0, 2, 97, 1, 0, 2, 97, 1, 0, 2, 97, 1, 1) // )"
    } ,    
    {
      "title": "Evolution",
      "url": "/desert/docs/evolution/",
      "content": "Evolution One of the primary features of desert is to support data type evolution via its generic codec for ADTs, and with some other design decisions. In this section we review what kind of changes can be done to the data model without breaking binary compatibility. Tuples vs products As already mentioned on the codecs page, tuples and non-evolved case classes are binary compatible. This means that it is possible to convert a tuple to a case class without breaking the serialization format: import io.github.vigoo.desert._ import io.github.vigoo.desert.codecs._ import io.github.vigoo.desert.syntax._ case class Point(x: Int, y: Int) object Point { implicit val codec: BinaryCodec[Point] = BinaryCodec.derive() } val ex1 = for { data1 &lt;- serializeToArray((5, 6)) pt1 &lt;- deserializeFromArray[Point](data1) data2 &lt;- serializeToArray(pt1) tup2 &lt;- deserializeFromArray[(Int, Int)](data2) } yield (pt1, tup2) // ex1: Either[DesertFailure, (Point, (Int, Int))] = Right( // value = (Point(x = 5, y = 6), (5, 6)) // ) Primitives vs wrappers Codecs derived with deriveForWrapper are also fully compatible with their underlying primitive type, so it is fully safe to evolve the date model to make use of more and more value type wrappers: case class Id(id: Int) // extends AnyVal // extends AnyVal object Id { implicit val codec: BinaryCodec[Id] = BinaryCodec.deriveForWrapper } val ex2 = for { data1 &lt;- serializeToArray(3) id1 &lt;- deserializeFromArray[Id](data1) data2 &lt;- serializeToArray(id1) raw1 &lt;- deserializeFromArray[Int](data2) } yield (id1, raw1) // ex2: Either[DesertFailure, (Id, Int)] = Right(value = (Id(id = 3), 3)) Collections Collection codecs share their binary representation via iterableCodec and they can be freely replaced: val ex3 = for { data1 &lt;- serializeToArray(List(1, 2, 3)) set1 &lt;- deserializeFromArray[Set[Int]](data1) data2 &lt;- serializeToArray(set1) vec1 &lt;- deserializeFromArray[Vector[Int]](data2) } yield (set1, vec1) // ex3: Either[DesertFailure, (Set[Int], Vector[Int])] = Right( // value = (Set(1, 2, 3), Vector(1, 2, 3)) // ) Adding a new field If a case class gets extended by a new field, it has to be explicitly marked with an evolution step passed as a parameter to the derive function. For addig a new field, this defines a default value to be used when deserializing an old data. With this the old and the new data model remains fully compatible: If the old version reads a new data, it skips the new field If the new version reads an old data, it uses the provided default to set the field case class PointV1(x: Int, y: Int) object PointV1 { implicit val codec: BinaryCodec[PointV1] = BinaryCodec.derive() } case class PointV2(x: Int, y: Int, z: Int) object PointV2 { implicit val codec: BinaryCodec[PointV2] = BinaryCodec.derive(FieldAdded[Int](\"z\", 1)) } val ex4 = for { oldData &lt;- serializeToArray(PointV1(10, 20)) newPt &lt;- deserializeFromArray[PointV2](oldData) newData &lt;- serializeToArray(newPt) oldPt &lt;- deserializeFromArray[PointV1](newData) } yield (oldPt, newPt) // ex4: Either[DesertFailure, (PointV1, PointV2)] = Right( // value = (PointV1(x = 10, y = 20), PointV2(x = 10, y = 20, z = 1)) // ) Note that the added field does not have to be last position of the case class. Making a field optional Another supported evolution step is making a field optional. This can be an intermediate step before completely removing an obsolete field. Once again this change has to be recorded in the parameter list for derive. With that, we get the following capabilities: If the old version reads the new data, and it is Some(x), it will be read as x If the old version reads the new data, and it is None the serialization fails If the new version reads the old data, it automatically wraps the field in Some case class PointV3(x: Int, y: Int, z: Option[Int]) object PointV3 { implicit val codec: BinaryCodec[PointV3] = BinaryCodec.derive( FieldAdded[Int](\"z\", 1), FieldMadeOptional(\"z\") ) } val ex5 = for { v1Data &lt;- serializeToArray(PointV1(10, 20)) newPt &lt;- deserializeFromArray[PointV3](v1Data) newData &lt;- serializeToArray(newPt) v2Pt &lt;- deserializeFromArray[PointV2](newData) } yield (v2Pt, newPt) // ex5: Either[DesertFailure, (PointV2, PointV3)] = Right( // value = ( // PointV2(x = 10, y = 20, z = 1), // PointV3(x = 10, y = 20, z = Some(value = 1)) // ) // ) val ex5fail = for { v3Data &lt;- serializeToArray(PointV3(10, 20, None)) v2Pt &lt;- deserializeFromArray[PointV2](v3Data) } yield v2Pt // ex5fail: Either[DesertFailure, PointV2] = Left( // value = NonOptionalFieldSerializedAsNone(fieldName = \"z\") // ) Note that any field can be made optional, not just the one which was added later like in this example. Removing a field The third supported evolution step is removing a field. Here backward compatibility has a limitation: New version can read old data by simply skipping the removed field Old version can only read new data if the removed field was an Option[T], reading it as None case class PointV4(x: Int, y: Int) object PointV4 { implicit val codec: BinaryCodec[PointV4] = BinaryCodec.derive( FieldAdded[Int](\"z\", 1), FieldMadeOptional(\"z\"), FieldRemoved(\"z\") ) } val ex6 = for { v2Data &lt;- serializeToArray(PointV2(10, 20, 30)) newPt &lt;- deserializeFromArray[PointV4](v2Data) newData &lt;- serializeToArray(newPt) v3Pt &lt;- deserializeFromArray[PointV3](newData) } yield (v3Pt, newPt) // ex6: Either[DesertFailure, (PointV3, PointV4)] = Right( // value = (PointV3(x = 10, y = 20, z = None), PointV4(x = 10, y = 20)) // ) val ex6fail = for { v4Data &lt;- serializeToArray(PointV4(10, 20)) v2Pt &lt;- deserializeFromArray[PointV2](v4Data) } yield v2Pt // ex6fail: Either[DesertFailure, PointV2] = Left( // value = FieldRemovedInSerializedVersion(fieldName = \"z\") // ) Transient fields Adding a new transient field does not change the binary representation. Making an existing field transient can be recorded as an evolution step called FieldMadeTransient. It is just an alias for FieldRemoved from the serializer’s point of view, so the same rules apply as for field removal. case class PointV5(x: Int, @TransientField(0) y: Int) object PointV5 { implicit val codec: BinaryCodec[PointV5] = BinaryCodec.derive( FieldAdded[Int](\"z\", 1), FieldMadeOptional(\"z\"), FieldRemoved(\"z\"), FieldMadeTransient(\"y\") ) } val ex7 = for { v4Data &lt;- serializeToArray(PointV4(10, 20)) newPt &lt;- deserializeFromArray[PointV5](v4Data) } yield newPt // ex7: Either[DesertFailure, PointV5] = Right(value = PointV5(x = 10, y = 0)) val ex7fail = for { v5Data &lt;- serializeToArray(PointV5(10, 20)) v4Pt &lt;- deserializeFromArray[PointV4](v5Data) } yield v4Pt // ex7fail: Either[DesertFailure, PointV4] = Left( // value = FieldRemovedInSerializedVersion(fieldName = \"y\") // ) Adding a new constructor Adding a new constructor to a sealed trait is allowed, but it has to be added to the end of the list to maintain constructor ID order. For the same reason it is currently not supported to remove a constructor either. Each constructor can be evolved separately with the above methods as they have their own evolution steps. Adding a new transient constructor Constructors marked with TransientConstructor are not getting an associated constructor ID so they can be inserted or get removed freely. Type registry placeholders When using type identifiers with the type registry and a previously registered type has been removed from the code, it’s place can be kept assigned by using registerPlaceholder in the registration: val typeRegistry2 = DefaultTypeRegistry() .registerPlaceholder() // previously: .register[Impl1] .register[Impl2] .freeze() Generic product evolution encoding This section gives an overview of how the above described evolution steps are being encoded in the binary representation of case classes. Let’s examine the output of serializing the above examples! val v1 = serializeToArray(PointV1(100, 200)) // v1: Either[DesertFailure, Array[Byte]] = Right( // value = Array(0, 0, 0, 0, 100, 0, 0, 0, -56) // ) The first byte is the version and it is 0 because there are no evolution steps for this type. This makes it compatible with a (Int, Int) pair 0, 0, 0, 100 is the fixed 32-bit integer representation of 100 0, 0, 0, -56 is the fixed 32-bit integer representation of 200 So PointV1 is always serialized into 9 bytes val v2 = serializeToArray(PointV2(100, 200, 300)) // v2: Either[DesertFailure, Array[Byte]] = Right( // value = Array(1, 16, 8, 0, 0, 0, 100, 0, 0, 0, -56, 0, 0, 1, 44) // ) The first byte is version (or in other words, the number of evolution steps) which is now 1 Because the version is non-zero, the next item to read is a variable-width integer representing the chunk size of the original version-0 data In this case it takes 1 byte: 16 which represents 8, the sum of the two 32 bit fields x and y Following it the next item is again a variable-width integer representing the chunk size for the single newly added field z It is again 1 byte: 8 which means 4 The rest is the first chunk containing the fields from V1 and then the next chunk containing the field from V2 The old version can use the chunk size data to skip the unknown fields and only read the first one PointV2 takes 15 bytes by having 3 bytes of header and 12 bytes of data val v3 = serializeToArray(PointV3(100, 200, Some(300))) // v3: Either[DesertFailure, Array[Byte]] = Right( // value = Array(2, 16, 10, 1, 1, 0, 0, 0, 100, 0, 0, 0, -56, 1, 0, 0, 1, 44) // ) The first byte is now 2 as we added a new evolution step The header part starts with almost the same two values, encoded as 16, 10 The first chunk is still 8 byte long, but the next one is now 10 which means 5, because the Option wrapper adds an extra byte to differentiate None or Some After the two variable integers we have a third one corresponding to the third evolution step. But as it did not add any new fields, it is not a chunk size, but a special negative number marking the field as optional. This is the value 1 which in fact represents -1 in the variable-length integer encoding. This code is always followed by a serialized field position, which is again a variable-length integer, in this case 1 aka -1 again. Negative field positions address a non-first chunk Positive field positions address a field in the first chunk This is for space efficiency. The first chunk is kept flat without any chunk size info needed so non-evolved data type serialization is almost zero cost. The rest is the data itself, first chunk is still 8 bytes Second chunk is now 5 bytes, the 1 indicates that it is Some and the rest 4 bytes are the 32-bit integer PointV3 takes 18 bytes in total, 5 bytes of header and 13 bytes of data val v4 = serializeToArray(PointV4(100, 200)) // v4: Either[DesertFailure, Array[Byte]] = Right( // value = Array(3, 16, 0, 1, -128, 3, 2, 122, 0, 0, 0, 100, 0, 0, 0, -56) // ) The first byte is now 3 The first chunk size is still 16 because we removed the field from the second chunk The next integer still represents the chunk size for the second evolution step, which was adding the field z. But as it has been later removed, the chunk size is now 0 The third variable integer in the header part is 1 representing that a field has been removed. It is followed by the field position which in V3 was -1 pointing to the second chunk. But as that is removed now, it is a special value -128, which is FieldPosition.removed The fourth variable integer is again a special code, encoded as 3 representing the value -2. This is the code for field removal. It is followed by a serialized string containing the removed field’s name. String deduplication is enabled here as it was described in the codecs page. As this is the first occurrence of the string \"z\" it is encoded as 2, 122. The rest 8 bytes are the first chunk containing x and y So PointV4 in this example takes 16 bytes in total, where 8 bytes are header and 8 bytes are data. The exact size depends on the string IDs so it can change in real world situations."
    } ,    
    {
      "title": "Getting started",
      "url": "/desert/docs/",
      "content": "Getting started with desert Desert is a binary serialization library for Scala, focusing on creating small binaries while still enabling binary compatible evolution of the data model. It is suitable for use cases such as Akka remoting and persistence. First add desert as a dependency: libraryDependencies += \"io.github.vigoo\" %% \"desert-core\" % \"0.2.0\" and optionally some extra bindings: libraryDependencies += \"io.github.vigoo\" %% \"desert-akka\" % \"0.2.0\" libraryDependencies += \"io.github.vigoo\" %% \"desert-cats\" % \"0.2.0\" libraryDependencies += \"io.github.vigoo\" %% \"desert-cats-effect\" % \"0.2.0\" libraryDependencies += \"io.github.vigoo\" %% \"desert-zio\" % \"0.2.0\" The most simple use case is to serialize a known type to an array of bytes and read it back: import io.github.vigoo.desert._ import io.github.vigoo.desert.codecs._ import io.github.vigoo.desert.syntax._ val x = \"Hello world\" val dataOrFailure = serializeToArray(x) // dataOrFailure: Either[DesertFailure, Array[Byte]] = Right( // value = Array(22, 72, 101, 108, 108, 111, 32, 119, 111, 114, 108, 100) // ) val y = dataOrFailure.flatMap(data =&gt; deserializeFromArray[String](data)) // y: Either[DesertFailure, String] = Right(value = \"Hello world\") Codecs This works because there is an implicit BinaryCodec for String in scope, imported from the codecs package. Read the codecs page to learn about the available codecs and how to define custom ones. Low level input/output The above example shows the convenient functions to work on arrays directly, but they have a more generic version working on the low level BinaryInput and BinaryOutput interfaces. These are described on the input/output page. Evolution One of the primary features of the library is the support for evolving the data model. The possibilities are described on a separate page. Type registry For cases when the exact type to be deserialized is not known at compile type, the possibilities can be registered to a type registry. Integrations There are three additional modules providing integrations to different environments: Akka codecs and Akka Serializer implementation Cats Effect defining the serialization/deserialization as an effect ZIO defining the serialization/deserialization as an effect and some codecs"
    } ,    
    {
      "title": "desert: Home",
      "url": "/desert/",
      "content": ""
    } ,    
    {
      "title": "Input/output",
      "url": "/desert/docs/input-output/",
      "content": "Binary input/output The desert library uses the BinaryInput and BinaryOutput interfaces to read and write data. The default implementation is JavaStreamBinaryInput and JavaStreamBinaryOutput. These are simple wrappers on top of Java IO streams, capturing failures and adding some extra functionality. Variable length integer encoding One extra feature is the variable length integer encoding which was borrowed from the Kryo library. It encodes 32 bit integers in 1-5 bytes. It is used for all “integer id like” data within the library. import java.io.ByteArrayOutputStream import io.github.vigoo.desert._ val stream = new ByteArrayOutputStream() // stream: ByteArrayOutputStream = ����\u000f val output = new JavaStreamBinaryOutput(stream) // output: JavaStreamBinaryOutput = io.github.vigoo.desert.JavaStreamBinaryOutput@43280782 output.writeVarInt(1, optimizeForPositive = true) // res0: Either[DesertFailure, Unit] = Right(value = ()) val r1 = stream.toByteArray // r1: Array[Byte] = Array(1) stream.reset() output.writeVarInt(4096, optimizeForPositive = true) // res2: Either[DesertFailure, Unit] = Right(value = ()) val r2 = stream.toByteArray // r2: Array[Byte] = Array(-128, 32) stream.reset() output.writeVarInt(-1, optimizeForPositive = true) // res4: Either[DesertFailure, Unit] = Right(value = ()) val r3 = stream.toByteArray // r3: Array[Byte] = Array(-1, -1, -1, -1, 15) [Z]IO? The read and write operations on these interfaces are not encapsulated to any effect type like Cats Effect’s IO or ZIO, they are just Either[DesertFailure, T]. This is for performance reasons and also for being independent of the chosen library at this low level. On the other hand the Cats Effect module and the ZIO module both define IO versions of the higher level serialization/deserialization functions. Usage of desert should be in low level enough to treat these operations as a single effect."
    } ,      
    {
      "title": "Release notes",
      "url": "/desert/docs/release-notes/",
      "content": "Release notes 0.2.0 Core migrated to ZPure Codecs for cats data types moved to desert-cats module 0.1.5 Performance improvements 0.1.4 Fixed the akka serialization base class 0.1.3 Fixed stack safety 0.1.2 Disabled automatic string deduplication ZIO 1.0.0 0.1.1 Support for making fields and constructors transient Ability to pass offset and length to writeBytes in custom serializers General purpose serialization failure type (SerializationFailure) for custom serializers Ability to read/write byte arrays using ZIP compression UUID codec Helper functions to lift Try[T] into Ser[T] and Deser[T] for custom serializers 0.1.0 The initial release of desert"
    } ,      
    {
      "title": "Type Registry",
      "url": "/desert/docs/type-registry/",
      "content": "Type Registry There are cases when the exact type to be deserialized is not known at compile time. In this case an additional type ID must be serialized to the data stream in order to being able to select the correct deserializer when reading it back. desert is not doing any automatic reflection based type identification. All the types that can participate in the above described scenario must be explicitly registered to a type registry: import io.github.vigoo.desert._ import io.github.vigoo.desert.codecs._ import io.github.vigoo.desert.syntax._ case class TestProd(value: Int) object TestProd { implicit val codec: BinaryCodec[TestProd] = BinaryCodec.derive() } val typeRegistry1 = DefaultTypeRegistry() .register[String] .register[TestProd] .freeze() // typeRegistry1: TypeRegistry = io.github.vigoo.desert.DefaultTypeRegistry@715e35c5 In this example we register two types, String and a custom case class TestProd. We can then use this registry to try serializing a value of AnyRef. If the type matches any of the registered ones, it will succeed: val dataOrFailure1 = serializeUnknownToArray(\"Hello world\", typeRegistry1) // dataOrFailure1: Either[DesertFailure, Array[Byte]] = Right( // value = Array(1, 22, 72, 101, 108, 108, 111, 32, 119, 111, 114, 108, 100) // ) val dataOrFailure2 = serializeUnknownToArray(TestProd(11), typeRegistry1) // dataOrFailure2: Either[DesertFailure, Array[Byte]] = Right( // value = Array(2, 0, 0, 0, 0, 11) // ) Both data streams start with a compact type ID (in this case it is 1 and 2) so when deserializing them, the library knows which codec to use: val x = dataOrFailure1.flatMap(data1 =&gt; deserializeUnknownFromArray(data1, typeRegistry1)) // x: Either[DesertFailure, Any] = Right(value = \"Hello world\") val y = dataOrFailure2.flatMap(data2 =&gt; deserializeUnknownFromArray(data2, typeRegistry1)) // y: Either[DesertFailure, Any] = Right(value = TestProd(value = 11)) Unknowns in fields These functions (and the similar ones working on BinaryInput and BinaryOutput instances) are good when the top-level type is unknown, like when serializing arbitrary actor messages. But what if a field’s type is an open trait? For this the BinaryCodec.unknown function can be used to automatically use the above described mechanisms. Let’s see an example: trait Iface object Iface { implicit val codec: BinaryCodec[Iface] = BinaryCodec.unknown } case class Impl1(x: Int) extends Iface object Impl1 { implicit val codec: BinaryCodec[Impl1] = BinaryCodec.derive() } case class Impl2(x: Int) extends Iface object Impl2 { implicit val codec: BinaryCodec[Impl2] = BinaryCodec.derive() } case class Outer(inner: Iface) object Outer { implicit val codec: BinaryCodec[Outer] = BinaryCodec.derive() } val typeRegistry2 = DefaultTypeRegistry() .register[Impl1] .register[Impl2] .freeze() // typeRegistry2: TypeRegistry = io.github.vigoo.desert.DefaultTypeRegistry@4c97a802 val dataOrFailure = serializeToArray(Outer(Impl2(11)), typeRegistry2) // dataOrFailure: Either[DesertFailure, Array[Byte]] = Right( // value = Array(0, 2, 0, 0, 0, 0, 11) // ) val result = dataOrFailure.flatMap(data =&gt; deserializeFromArray[Outer](data, typeRegistry2)) // result: Either[DesertFailure, Outer] = Right( // value = Outer(inner = Impl2(x = 11)) // )"
    } ,    
    {
      "title": "ZIO",
      "url": "/desert/docs/zio/",
      "content": "ZIO integration The zio module contains codecs for the ZIO Chunk type and wraps the top level serialization functions in ZIO effects. To use it add the dependency: libraryDependencies += \"io.github.vigoo\" %% \"desert-zio\" % \"0.2.0\" Syntax Import the zio-specific syntax module: import io.github.vigoo.desert._ import io.github.vigoo.desert.codecs._ import io.github.vigoo.desert.zio.syntax._ to get variants like def serializeToChunk[T: BinarySerializer](value: T, typeRegistry: TypeRegistry = TypeRegistry.empty): ZIO[Any, DesertFailure, Chunk[Byte]] Codecs There is a generic Chunk codec built on top of iterableCodec so it is binary compatible with other collections. It is also specialized for Chunk[Byte] so it directly uses the input/output interfaces ‘readBytes’ and ‘writeBytes’ functions for higher performance."
    } ,        
  ];

  idx = lunr(function () {
    this.ref("title");
    this.field("content");

    docs.forEach(function (doc) {
      this.add(doc);
    }, this);
  });

  docs.forEach(function (doc) {
    docMap.set(doc.title, doc.url);
  });
}

// The onkeypress handler for search functionality
function searchOnKeyDown(e) {
  const keyCode = e.keyCode;
  const parent = e.target.parentElement;
  const isSearchBar = e.target.id === "search-bar";
  const isSearchResult = parent ? parent.id.startsWith("result-") : false;
  const isSearchBarOrResult = isSearchBar || isSearchResult;

  if (keyCode === 40 && isSearchBarOrResult) {
    // On 'down', try to navigate down the search results
    e.preventDefault();
    e.stopPropagation();
    selectDown(e);
  } else if (keyCode === 38 && isSearchBarOrResult) {
    // On 'up', try to navigate up the search results
    e.preventDefault();
    e.stopPropagation();
    selectUp(e);
  } else if (keyCode === 27 && isSearchBarOrResult) {
    // On 'ESC', close the search dropdown
    e.preventDefault();
    e.stopPropagation();
    closeDropdownSearch(e);
  }
}

// Search is only done on key-up so that the search terms are properly propagated
function searchOnKeyUp(e) {
  // Filter out up, down, esc keys
  const keyCode = e.keyCode;
  const cannotBe = [40, 38, 27];
  const isSearchBar = e.target.id === "search-bar";
  const keyIsNotWrong = !cannotBe.includes(keyCode);
  if (isSearchBar && keyIsNotWrong) {
    // Try to run a search
    runSearch(e);
  }
}

// Move the cursor up the search list
function selectUp(e) {
  if (e.target.parentElement.id.startsWith("result-")) {
    const index = parseInt(e.target.parentElement.id.substring(7));
    if (!isNaN(index) && (index > 0)) {
      const nextIndexStr = "result-" + (index - 1);
      const querySel = "li[id$='" + nextIndexStr + "'";
      const nextResult = document.querySelector(querySel);
      if (nextResult) {
        nextResult.firstChild.focus();
      }
    }
  }
}

// Move the cursor down the search list
function selectDown(e) {
  if (e.target.id === "search-bar") {
    const firstResult = document.querySelector("li[id$='result-0']");
    if (firstResult) {
      firstResult.firstChild.focus();
    }
  } else if (e.target.parentElement.id.startsWith("result-")) {
    const index = parseInt(e.target.parentElement.id.substring(7));
    if (!isNaN(index)) {
      const nextIndexStr = "result-" + (index + 1);
      const querySel = "li[id$='" + nextIndexStr + "'";
      const nextResult = document.querySelector(querySel);
      if (nextResult) {
        nextResult.firstChild.focus();
      }
    }
  }
}

// Search for whatever the user has typed so far
function runSearch(e) {
  if (e.target.value === "") {
    // On empty string, remove all search results
    // Otherwise this may show all results as everything is a "match"
    applySearchResults([]);
  } else {
    const tokens = e.target.value.split(" ");
    const moddedTokens = tokens.map(function (token) {
      // "*" + token + "*"
      return token;
    })
    const searchTerm = moddedTokens.join(" ");
    const searchResults = idx.search(searchTerm);
    const mapResults = searchResults.map(function (result) {
      const resultUrl = docMap.get(result.ref);
      return { name: result.ref, url: resultUrl };
    })

    applySearchResults(mapResults);
  }

}

// After a search, modify the search dropdown to contain the search results
function applySearchResults(results) {
  const dropdown = document.querySelector("div[id$='search-dropdown'] > .dropdown-content.show");
  if (dropdown) {
    //Remove each child
    while (dropdown.firstChild) {
      dropdown.removeChild(dropdown.firstChild);
    }

    //Add each result as an element in the list
    results.forEach(function (result, i) {
      const elem = document.createElement("li");
      elem.setAttribute("class", "dropdown-item");
      elem.setAttribute("id", "result-" + i);

      const elemLink = document.createElement("a");
      elemLink.setAttribute("title", result.name);
      elemLink.setAttribute("href", result.url);
      elemLink.setAttribute("class", "dropdown-item-link");

      const elemLinkText = document.createElement("span");
      elemLinkText.setAttribute("class", "dropdown-item-link-text");
      elemLinkText.innerHTML = result.name;

      elemLink.appendChild(elemLinkText);
      elem.appendChild(elemLink);
      dropdown.appendChild(elem);
    });
  }
}

// Close the dropdown if the user clicks (only) outside of it
function closeDropdownSearch(e) {
  // Check if where we're clicking is the search dropdown
  if (e.target.id !== "search-bar") {
    const dropdown = document.querySelector("div[id$='search-dropdown'] > .dropdown-content.show");
    if (dropdown) {
      dropdown.classList.remove("show");
      document.documentElement.removeEventListener("click", closeDropdownSearch);
    }
  }
}
