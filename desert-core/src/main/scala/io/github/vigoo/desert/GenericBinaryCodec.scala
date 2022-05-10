package io.github.vigoo.desert

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

import io.github.vigoo.desert.BinaryDeserializer.{Deser, DeserializationEnv}
import io.github.vigoo.desert.BinaryDeserializerOps._
import io.github.vigoo.desert.BinarySerializer.{Ser, SerializationEnv}
import io.github.vigoo.desert.BinarySerializerOps._
import io.github.vigoo.desert.GenericBinaryCodec._
import io.github.vigoo.desert.codecs._
import shapeless.labelled._
import shapeless.ops.hlist._
import shapeless.tag._
import shapeless.{:+:, _}
import _root_.zio.prelude.fx._

import scala.reflect.ClassTag

/**
 * Trait containing the more generic implicits backing up the generic binary codec
 *
 * These are the default implementations while in [[GenericDerivationApi]] there are some more specialized ones
 * for handling optional and transient fields / constructors.
 */
trait LowerPriorityGenericDerivationApi {
  implicit def hlistSerializer[K <: Symbol, H, T <: HList](implicit witness: Witness.Aux[K],
                                                           headCodec: Lazy[BinaryCodec[H]],
                                                           tailCodec: ChunkedBinarySerializer[T]): ChunkedBinarySerializer[FieldType[K, H] :: T]

  implicit def hlistDeserializer[K <: Symbol, H, T <: HList](implicit witness: Witness.Aux[K],
                                                             headCodec: Lazy[BinaryCodec[H]],
                                                             tailCodec: ChunkedBinaryDeserializer[T]): ChunkedBinaryDeserializer[FieldType[K, H] :: T]

  implicit def clistSerializer[K <: Symbol, H, T <: Coproduct](implicit witness: Witness.Aux[K],
                                                               headCodec: Lazy[BinaryCodec[H]],
                                                               tailCodec: ChunkedBinarySerializer[T]): ChunkedBinarySerializer[FieldType[K, H] :+: T]

  implicit def clistDeserializer[K <: Symbol, H, T <: Coproduct](implicit witness: Witness.Aux[K],
                                                                 headCodec: Lazy[BinaryCodec[H]],
                                                                 tailCodec: ChunkedBinaryDeserializer[T]): ChunkedBinaryDeserializer[FieldType[K, H] :+: T]
}

/**
 * Trait containing the public interface for the generic binary codec.
 *
 * As there is no single global generic derivation implementation because it is parameteric with the evolution steps,
 * a specific type's codec derviation is possible by accessing the necessary implicits (for HLists and Coproducts) via
 * this interface.
 *
 * The two type classes implemented for both products and coproducts are [[ChunkedBinarySerializer]] and
 * [[ChunkedBinaryDeserializer]]. These monads are more specific than the [[BinarySerializer]] and [[BinaryDeserializer]],
 * holding state specific for a single evolvable value's serialization.
 */
trait GenericDerivationApi extends LowerPriorityGenericDerivationApi {
  implicit val hnilSerializer: ChunkedBinarySerializer[HNil]
  implicit val hnilDeserializer: ChunkedBinaryDeserializer[HNil]
  implicit val cnilSerializer: ChunkedBinarySerializer[CNil]
  implicit val cnilDeserializer: ChunkedBinaryDeserializer[CNil]


  implicit def hlistTransientSerializer[K <: Symbol, H, T <: HList](implicit witness: Witness.Aux[K],
                                                                    tailCodec: ChunkedBinarySerializer[T]): ChunkedBinarySerializer[FieldType[K, MarkedAsTransient[H]] :: T]

  implicit def hlistOptionalDeserializer[K <: Symbol, H, T <: HList](implicit witness: Witness.Aux[K],
                                                                     headCodec: Lazy[BinaryCodec[H]],
                                                                     optHeadCodec: Lazy[BinaryCodec[Option[H]]],
                                                                     tailCodec: ChunkedBinaryDeserializer[T]): ChunkedBinaryDeserializer[FieldType[K, Option[H]] :: T]

  implicit def hlistTransientDeserializer[K <: Symbol, H, T <: HList](implicit witness: Witness.Aux[K],
                                                                      tailCodec: ChunkedBinaryDeserializer[T]): ChunkedBinaryDeserializer[FieldType[K, MarkedAsTransient[H]] :: T]

  implicit def clistTransientSerializer[K <: Symbol, H, T <: Coproduct](implicit witness: Witness.Aux[K],
                                                                        tailCodec: ChunkedBinarySerializer[T]): ChunkedBinarySerializer[FieldType[K, MarkedAsTransient[H]] :+: T]

  implicit def clistTransientDeserializer[K <: Symbol, H, T <: Coproduct](implicit witness: Witness.Aux[K],
                                                                          tailCodec: ChunkedBinaryDeserializer[T]): ChunkedBinaryDeserializer[FieldType[K, MarkedAsTransient[H]] :+: T]

  /**
   * Type class to tag a labelled generic representation of a type with a type level information about whether each
   * field or constructor is marked as transient or not.
   *
   * For products 'H' is a [[HList]], for coproducts it is a [[Coproduct]], with each item being tagged with the
   * field or constructor's name by [[LabelledGeneric]], having a type of 'FieldType[K, V]'.
   *
   * The result of this type-level operation, 'Result' is a [[HList]] or [[Coproduct]] where those fields or constructors
   * that are having a transient attribute of [[TransientField]] or [[TransientConstructor]] are also tagged with
   * the [[MarkedAsTransient]] tag, so each element is either 'FieldType[K, V]' or 'FieldType[K, MarkedAsTransient[V]]'.
   *
   * This makes it possible to differentiate in compile time between transient and non-transient flags in the implementation
   * of the [[ChunkedBinaryDeserializer]] and [[ChunkedBinarySerializer]] type classes.
   *
   * @tparam H The generic representation of the type
   * @tparam AF [[HList]] of the [[TransientField]] annotations, extracted by [[Annotations]]
   * @tparam AC [[HList]] of the [[TransientConstructor]] annotations, extracted by [[Annotations]]
   */
  trait TagTransients[H, AF, AC] {
    type Result

    /**
     * Create a representation of the labelled generic value that also has transient tags
     */
    def tag(value: H): Result

    /**
     * Removes the transient tags from a labelled generic representation
     */
    def untag(value: Result): H
  }

  /**
   * Implementation of [[TagTransients]] for both products and coproducts
   */
  object TagTransients {
    type Aux[H, AF, AC, R] = TagTransients[H, AF, AC] { type Result = R }

    implicit def hnilTagTransients[AC]: TagTransients.Aux[HNil, HNil, AC, HNil] = new TagTransients[HNil, HNil, AC] {
      override type Result = HNil
      override def tag(value: HNil): Result = HNil
      override def untag(value: HNil): HNil = HNil
    }

    implicit def cnilTagTransients[AT]: TagTransients.Aux[CNil, AT, HNil, CNil] = new TagTransients[CNil, AT, HNil] {
      override type Result = CNil
      override def tag(value: CNil): Result = ???
      override def untag(value: CNil): CNil = ???
    }

    implicit def prodTagTransientsTransient[K <: Symbol, H, T <: HList, TT <: HList, AT <: HList, AC]
    (implicit tailTagger: TagTransients.Aux[T, AT, AC, TT]): TagTransients.Aux[FieldType[K, H] :: T, Some[TransientField] :: AT, AC, FieldType[K, MarkedAsTransient[H]] :: TT] =
      new TagTransients[FieldType[K, H] :: T, Some[TransientField] :: AT, AC] {
        override type Result = FieldType[K, MarkedAsTransient[H]] :: TT
        override def tag(value: FieldType[K, H] :: T): Result = value match {
          case head :: tail =>
            head.asInstanceOf[FieldType[K, MarkedAsTransient[H]]] :: tailTagger.tag(tail)
        }
        override def untag(value: FieldType[K, MarkedAsTransient[H]] :: TT): FieldType[K, H] :: T = value match {
          case head :: tail =>
            head.asInstanceOf[FieldType[K, H]] :: tailTagger.untag(tail)
        }
      }

    implicit def prodTagTransientsPersistent[K <: Symbol, H, T <: HList, TT <: HList, AT <: HList, AC]
    (implicit tailTagger: TagTransients.Aux[T, AT, AC, TT]): TagTransients.Aux[FieldType[K, H] :: T, None.type :: AT, AC, FieldType[K, H] :: TT] =
      new TagTransients[FieldType[K, H] :: T, None.type :: AT, AC] {
        override type Result = FieldType[K, H] :: TT
        override def tag(value: FieldType[K, H] :: T): Result = value match {
          case head :: tail =>
            head :: tailTagger.tag(tail)
        }
        override def untag(value: FieldType[K, H] :: TT): FieldType[K, H] :: T = value match {
          case head :: tail =>
            head :: tailTagger.untag(tail)
        }
      }

    implicit def coprodTagTransientsPersistent[K <: Symbol, H, T <: Coproduct, TT <: Coproduct, AT, AC <: HList]
    (implicit tailTagger: TagTransients.Aux[T, AT, AC, TT]): TagTransients.Aux[FieldType[K, H] :+: T, AT, None.type :: AC, FieldType[K, H] :+: TT] =
      new TagTransients[FieldType[K, H] :+: T, AT, None.type :: AC] {
        override type Result = FieldType[K, H] :+: TT
        override def tag(value: FieldType[K, H] :+: T): Result = value match {
          case Inl(head) =>
            Inl(head)
          case Inr(tail) =>
            Inr(tailTagger.tag(tail))
        }
        override def untag(value: FieldType[K, H] :+: TT): FieldType[K, H] :+: T = value match {
          case Inl(head) =>
            Inl(head)
          case Inr(tail) =>
            Inr(tailTagger.untag(tail))
        }
      }

    implicit def coprodTagTransientsTransient[K <: Symbol, H, T <: Coproduct, TT <: Coproduct, AT, AC <: HList]
    (implicit tailTagger: TagTransients.Aux[T, AT, AC, TT]): TagTransients.Aux[FieldType[K, H] :+: T, AT, Some[TransientConstructor] :: AC, FieldType[K, MarkedAsTransient[H]] :+: TT] =
      new TagTransients[FieldType[K, H] :+: T, AT, Some[TransientConstructor] :: AC] {
        override type Result = FieldType[K, MarkedAsTransient[H]] :+: TT
        override def tag(value: FieldType[K, H] :+: T): Result = value match {
          case Inl(head) =>
            Inl(head.asInstanceOf[FieldType[K, MarkedAsTransient[H]]])
          case Inr(tail) =>
            Inr(tailTagger.tag(tail))
        }
        override def untag(value: FieldType[K, MarkedAsTransient[H]] :+: TT): FieldType[K, H] :+: T = value match {
          case Inl(head) =>
            Inl(head.asInstanceOf[FieldType[K, H]])
          case Inr(tail) =>
            Inr(tailTagger.untag(tail))
        }
      }
  }

  /**
   * Type class extracting the field names or constructor names of a labelled generic representation to a [[HList]] of [[Symbol]] values
   * @tparam H The [[LabelledGeneric]] representation of a type
   */
  trait Symbols[H] {
    type Result <: HList

    /**
     * Extracts the [[HList]] of [[Symbol]] values from the type-level labelled generic representation
     */
    def apply(): Result
  }

  /**
   * Implementation of the [[Symbols]] type class for both products and coproducts
   */
  object Symbols {
    type Aux[H, R <: HList] = Symbols[H] { type Result = R }

    implicit val hnilSymbols: Symbols.Aux[HNil, HNil] = new Symbols[HNil] {
      override type Result = HNil
      override def apply(): Result = HNil
    }

    implicit def hlistSymbols[K <: Symbol, H, T <: HList, TR <: HList](implicit witness: Witness.Aux[K],
                                                                       tailSymbols: Symbols.Aux[T, TR]): Symbols.Aux[FieldType[K, H] :: T, Symbol :: TR] =
      new Symbols[FieldType[K, H] :: T] {
        override type Result = Symbol :: TR
        override def apply(): Result = witness.value :: tailSymbols()
      }

    implicit val cnilSymbols: Symbols.Aux[CNil, HNil] = new Symbols[CNil] {
      override type Result = HNil
      override def apply(): Result = HNil
    }

    implicit def clistSymbols[K <: Symbol, H, T <: Coproduct, TR <: HList](implicit witness: Witness.Aux[K],
                                                                           tailSymbols: Symbols.Aux[T, TR]): Symbols.Aux[FieldType[K, H] :+: T, Symbol :: TR] =
      new Symbols[FieldType[K, H] :+: T] {
        override type Result = Symbol :: TR
        override def apply(): Result = witness.value :: tailSymbols()
      }
  }

  /**
   * Type class to extract the type-level generic constructor names to a run-time vector of strings
   * @tparam T The [[LabelledGeneric]] representation of a type tagged by [[TagTransients]]
   */
  trait ToConstructorMap[T] {
    val constructors: Vector[String]
  }

  /**
   * Lower priority implicits for the implementation of [[ToConstructorMap]].
   *
   * This enables capturing the [[MarkedAsTransient]] tagged constructors in the [[ToConstructorMap]]
   */
  trait ToConstructorMapLowPriority {
    implicit def clist[K <: Symbol, H, T <: Coproduct](implicit witness: Witness.Aux[K],
                                                       tail: ToConstructorMap[T]): ToConstructorMap[FieldType[K, H] :+: T] =
      new ToConstructorMap[FieldType[K, H] :+: T] {
        val constructors: Vector[String] = witness.value.name +: tail.constructors
      }
  }

  /**
   * Implementation of the [[ToConstructorMap]] type class for both products and coproducts.
   *
   * For products the result is always an empty vector.
   * For coproducts the result if the vector of constructor names, except the ones marked as transient.
   */
  object ToConstructorMap extends ToConstructorMapLowPriority {
    implicit val cnil: ToConstructorMap[CNil] = new ToConstructorMap[CNil] { val constructors: Vector[String] = Vector.empty }
    implicit def clistTransient[K <: Symbol, H, T <: Coproduct](implicit witness: Witness.Aux[K],
                                                                tail: ToConstructorMap[T]): ToConstructorMap[FieldType[K, MarkedAsTransient[H]] :+: T] =
      new ToConstructorMap[FieldType[K, MarkedAsTransient[H]] :+: T] {
        val constructors: Vector[String] = tail.constructors
      }
    implicit val hnil: ToConstructorMap[HNil] = new ToConstructorMap[HNil] { val constructors: Vector[String] = Vector.empty }
    implicit def hlist[H <: HList]: ToConstructorMap[H] = new ToConstructorMap[H] { val constructors: Vector[String] = Vector.empty }
  }

  /**
   * The entry point of deriving a generic binary codec for a type 'T'
   *
   * The derivation needs both type level and runtime information about the type it derives the codec for.
   * On type level first the [[LabelledGeneric]] representation gets extracted into 'H'. This is either a [[HList]]
   * of fields or a [[Coproduct]] of constructors. Then with [[Annotations]] it finds both the [[TransientField]]
   * and [[TransientConstructor]] annotations for each field and constructor, stored in the 'Trs' and 'Trcs' types.
   * Based on these the [[TagTransients]] type level operation constructs 'TH' where each field/constructor is
   * tagged with its label and optionally as transient. This is the type which is then used recursively to
   * generate the serializer and the deserializer through the implicits of [[ChunkedBinarySerializer]] and
   * [[ChunkedBinaryDeserializer]].
   *
   * For these serializer and deserializer operations we also need some runtime data. The [[ToConstructorMap]]
   * type class extracts the name of non-transient constructors into a runtime [[Vector]]. This defines the
   * association between constructors and numeric IDs.
   *
   * It is not enough to have transient tags in type level because the [[TransientField]] annotation also holds
   * a default value. For this reason we need to be able to access the default value runtime as well, during
   * the construction of the generic representation in the deserializer.
   *
   * With 'keys', 'zip' and 'toList' we zip together the field names with the annotations and convert the result
   * to a runtime [[Map]].
   *
   * @param gen Extracts the labelled generic representation of type 'T' into 'H'
   * @param keys Gets the HList of field/constructor names of 'H' into 'Ks'
   * @param transientAnnotations Extracts the [[TransientField]] annotations of 'T' into 'Trs'
   * @param transientConstructorAnnotations Extracts the [[TransientConstructor]] annotations of 'T' into 'Trcs'
   * @param taggedTransients Tags the generic representation 'H' with transient tags based on 'Trs' and 'Trcs' into 'TH'
   * @param zip Creates a zipped list of 'Ks' and 'Trs', associating field names with transient annotations into 'KsTrs'
   * @param toList Extractor of the 'KsTrs' [[HList]] into a [[List]] of [[Symbol]] and optional [[TransientField]] pairs
   * @param serializer The serializer implementation for the tagged generic representation 'TH'
   * @param deserializer The deserializer implementation for the tagged generic representation 'TH'
   * @param toConstructorMap Extracts the names of the constructors of a coproduct from the generic representation 'TH' into a runtime vector
   * @param classTag Class tag for the type 'T'
   * @tparam T Type to derive the generic representation for
   * @tparam H Labelled generic representation of type 'T'
   * @tparam Ks [[HList]] of field/constructor names
   * @tparam Trs [[HList]] of optional [[TransientField]] annotations per field/constructor
   * @tparam Trcs [[HList]] of optional [[TransientConstructor]] annotations per field/constructor
   * @tparam KsTrs [[HList]] of pairs of [[Symbol]] and optional [[TransientField]] values
   * @tparam TH Tagged generic representation of type 'T' produced by [[TagTransients]]
   * @return The derived [[BinaryCodec]] for type 'T'
   */
  def derive[T, H, Ks <: HList, Trs <: HList, Trcs <: HList, KsTrs <: HList, TH]
    (implicit gen: LabelledGeneric.Aux[T, H],
     keys: Lazy[Symbols.Aux[H, Ks]],
     transientAnnotations: Annotations.Aux[TransientField, T, Trs],
     transientConstructorAnnotations: Annotations.Aux[TransientConstructor, T, Trcs],
     taggedTransients: TagTransients.Aux[H, Trs, Trcs, TH],
     zip: Zip.Aux[Ks :: Trs :: HNil, KsTrs],
     toList: ToTraversable.Aux[KsTrs, List, (Symbol, Option[TransientField])],
     serializer: Lazy[ChunkedBinarySerializer[TH]],
     deserializer: Lazy[ChunkedBinaryDeserializer[TH]],
     toConstructorMap: Lazy[ToConstructorMap[TH]],
     classTag: ClassTag[T]): BinaryCodec[T]
}

class GenericBinaryCodec(evolutionSteps: Vector[Evolution]) extends GenericDerivationApi {
  private val version: Byte = (evolutionSteps.size - 1).toByte
  private val fieldGenerations: Map[String, Byte] =
    evolutionSteps
      .zipWithIndex
      .collect {
        case (FieldAdded(name, _), idx) => (name, idx)
      }
      .map { case (name, idx) => (name, idx.toByte) }
      .toMap
  private val fieldDefaults: Map[String, Any] =
    evolutionSteps
      .collect {
        case FieldAdded(name, default) => (name, default)
      }
      .toMap
  private val madeOptionalAt: Map[String, Byte] =
    evolutionSteps
      .zipWithIndex
      .collect {
        case (FieldMadeOptional(name), idx) => (name, idx)
      }
      .map { case (name, idx) => (name, idx.toByte) }
      .toMap
  private val removedFields: Set[String] =
    evolutionSteps
      .collect {
        case FieldRemoved(name) => name
      }.toSet

  implicit val hnilSerializer: ChunkedBinarySerializer[HNil] =
    (_: HNil) => ChunkedSerOps.unit

  implicit val hnilDeserializer: ChunkedBinaryDeserializer[HNil] =
    () => ChunkedDeserOps.pure(HNil)

  implicit val cnilSerializer: ChunkedBinarySerializer[CNil] = { _ => ??? }
  implicit val cnilDeserializer: ChunkedBinaryDeserializer[CNil] = { () => ??? }

  implicit def hlistTransientSerializer[K <: Symbol, H, T <: HList](implicit witness: Witness.Aux[K],
                                                                    tailCodec: ChunkedBinarySerializer[T]): ChunkedBinarySerializer[FieldType[K, MarkedAsTransient[H]] :: T] = {
    case _ :: tailValues => tailCodec.serialize(tailValues)
  }

  implicit def hlistSerializer[K <: Symbol, H, T <: HList](implicit witness: Witness.Aux[K],
                                                           headCodec: Lazy[BinaryCodec[H]],
                                                           tailCodec: ChunkedBinarySerializer[T]): ChunkedBinarySerializer[FieldType[K, H] :: T] = {
    case headValue :: tailValues =>
      for {
        chunkedOutput <- ChunkedSerOps.getChunkedOutput
        fieldName = witness.value.name
        chunk = fieldGenerations.getOrElse(fieldName, 0: Byte)
        output = chunkedOutput.outputFor(chunk)
        _ <- ChunkedSerOps.fromSer(
            headCodec.value.serialize(headValue),
            output
        )
        _ <- ChunkedSerOps.recordFieldIndex(fieldName, chunk)
        _ <- tailCodec.serialize(tailValues)
      } yield ()
  }

  private def readOptionalFieldIfExists[H](fieldName: String)
                                          (implicit headCodec: Lazy[BinaryCodec[H]],
                                           optHeadCodec: Lazy[BinaryCodec[Option[H]]]): ChunkedDeser[Option[H]] = {
    ChunkedDeserOps.getChunkedInput.flatMap { chunkedInput =>
      ChunkedDeserOps.getChunkedState.flatMap { chunkedState =>
        if (chunkedInput.removedFields.contains(fieldName)) {
          ChunkedDeserOps.pure(None)
        } else {
          val chunk = fieldGenerations.getOrElse(fieldName, 0: Byte)
          val optSince = madeOptionalAt.getOrElse(fieldName, 0: Byte)

          ChunkedDeserOps.recordFieldIndex(fieldName, chunk).flatMap { fieldPosition =>
            if (chunkedInput.storedVersion < chunk) {
              // This field was not serialized
              fieldDefaults.get(fieldName) match {
                case Some(value) =>
                  if (optSince <= chunk) {
                    // It was originally Option[H]
                    ChunkedDeserOps.pure(value.asInstanceOf[Option[H]])
                  } else {
                    // It was made optional after it was added
                    ChunkedDeserOps.pure(Some(value.asInstanceOf[H]))
                  }
                case None =>
                  ChunkedDeserOps.failWith(DeserializationFailure(s"Field $fieldName is not in the stream and does not have default value", None))

              }
            } else {
              // This field was serialized
              if (chunkedInput.storedVersion < optSince) {
                // Expect H in the input stream and wrap with Some()
                for {
                  input <- ChunkedDeserOps.fromEither(chunkedInput.inputFor(chunk))
                  headValue <- ChunkedDeserOps.fromDeser(headCodec.value.deserialize(), input)
                } yield Some(headValue)
              } else {
                // Expect Option[H] in the input stream
                for {
                  input <- ChunkedDeserOps.fromEither(chunkedInput.inputFor(chunk))
                  headValue <- ChunkedDeserOps.fromDeser(optHeadCodec.value.deserialize(), input)
                } yield headValue
              }
            }
          }
        }
      }
    }
  }

  private def readFieldIfExists[H](fieldName: String)
                                  (implicit headCodec: Lazy[BinaryCodec[H]]): ChunkedDeser[H] = {
    ChunkedDeserOps.getChunkedInput.flatMap { chunkedInput =>
      ChunkedDeserOps.getChunkedState.flatMap { chunkedState =>
        // Check if field was removed
        if (chunkedInput.removedFields.contains(fieldName)) {
          ChunkedDeserOps.failWith(FieldRemovedInSerializedVersion(fieldName))
        } else {
          val chunk = fieldGenerations.getOrElse(fieldName, 0: Byte)
          ChunkedDeserOps.recordFieldIndex(fieldName, chunk).flatMap { fieldPosition =>
            if (chunkedInput.storedVersion < chunk) {
              // Field was not serialized
              fieldDefaults.get(fieldName) match {
                case Some(value) =>
                  ChunkedDeserOps.pure(value.asInstanceOf[H])
                case None =>
                  ChunkedDeserOps.failWith(FieldWithoutDefaultValueIsMissing(fieldName))
              }
            } else {
              // Field was serialized

              if (chunkedInput.madeOptionalAt.contains(fieldPosition)) {
                // The field was made optional in by a newer version, reading as Option[H]
                for {
                  input <- ChunkedDeserOps.fromEither(chunkedInput.inputFor(chunk))
                  isDefined <- ChunkedDeserOps.fromDeser(booleanCodec.deserialize(), input)
                  headValue <- if (isDefined) {
                    ChunkedDeserOps.fromDeser(headCodec.value.deserialize(), input)
                  } else {
                    ChunkedDeserOps.failWith(NonOptionalFieldSerializedAsNone(fieldName))
                  }
                } yield headValue
              } else {
                // Default case, reading the field from the given chunk
                for {
                  input <- ChunkedDeserOps.fromEither(chunkedInput.inputFor(chunk))
                  headValue <- ChunkedDeserOps.fromDeser(headCodec.value.deserialize(), input)
                } yield headValue
              }
            }
          }
        }
      }
    }
  }

  implicit def hlistTransientDeserializer[K <: Symbol, H, T <: HList](implicit witness: Witness.Aux[K],
                                                                      tailCodec: ChunkedBinaryDeserializer[T]): ChunkedBinaryDeserializer[FieldType[K, MarkedAsTransient[H]] :: T] =
    () => for {
      chunkedState <- ChunkedDeserOps.getChunkedState
      fieldName = witness.value.name
      headValue <- chunkedState.transientFields.get(Symbol(fieldName)) match {
        case Some(value) =>
          ChunkedDeserOps.pure(value)
        case None =>
          ChunkedDeserOps.failWith(DeserializationFailure(s"Illegal state while processing transient field $fieldName", None))
      }
      tailValues <- tailCodec.deserialize()
    } yield field[K](headValue.asInstanceOf[MarkedAsTransient[H]]) :: tailValues

  implicit def hlistOptionalDeserializer[K <: Symbol, H, T <: HList](implicit witness: Witness.Aux[K],
                                                                     headCodec: Lazy[BinaryCodec[H]],
                                                                     optHeadCodec: Lazy[BinaryCodec[Option[H]]],
                                                                     tailCodec: ChunkedBinaryDeserializer[T]): ChunkedBinaryDeserializer[FieldType[K, Option[H]] :: T] =
    () => {
      val fieldName = witness.value.name
      for {
        headValue <- readOptionalFieldIfExists[H](fieldName)
        tailValues <- tailCodec.deserialize()
      } yield field[K](headValue) :: tailValues
    }

  implicit def hlistDeserializer[K <: Symbol, H, T <: HList](implicit witness: Witness.Aux[K],
                                                             headCodec: Lazy[BinaryCodec[H]],
                                                             tailCodec: ChunkedBinaryDeserializer[T]): ChunkedBinaryDeserializer[FieldType[K, H] :: T] =
    () => {
      val fieldName = witness.value.name
      for {
        headValue <- readFieldIfExists(fieldName)
        tailValues <- tailCodec.deserialize()
      } yield field[K](headValue) :: tailValues
    }

  implicit def clistSerializer[K <: Symbol, H, T <: Coproduct](implicit witness: Witness.Aux[K],
                                                               headCodec: Lazy[BinaryCodec[H]],
                                                               tailCodec: ChunkedBinarySerializer[T]): ChunkedBinarySerializer[FieldType[K, H] :+: T] = {
    case Inl(headValue) =>
      for {
        chunkedOutput <- ChunkedSerOps.getChunkedOutput
        typeName = witness.value.name
        output = chunkedOutput.outputFor(0)
        constructorId <- ChunkedSerOps.getConstructorId(typeName)
        _ <- ChunkedSerOps.fromSer(
          writeVarInt(constructorId, optimizeForPositive = true) *>
          headCodec.value.serialize(headValue),
          output
        )
      } yield ()
    case Inr(tail) =>
      tailCodec.serialize(tail)
  }

  implicit def clistTransientSerializer[K <: Symbol, H, T <: Coproduct](implicit witness: Witness.Aux[K],
                                                                        tailCodec: ChunkedBinarySerializer[T]): ChunkedBinarySerializer[FieldType[K, MarkedAsTransient[H]] :+: T] = {
    case Inl(_) =>
      ChunkedSerOps.failWith(SerializingTransientConstructor(witness.value.name))
    case Inr(tail) =>
      tailCodec.serialize(tail)
  }

  implicit def clistDeserializer[K <: Symbol, H, T <: Coproduct](implicit witness: Witness.Aux[K],
                                                                 headCodec: Lazy[BinaryCodec[H]],
                                                                 tailCodec: ChunkedBinaryDeserializer[T]): ChunkedBinaryDeserializer[FieldType[K, H] :+: T] =
    () => for {
      chunkedInput <- ChunkedDeserOps.getChunkedInput
      input <- ChunkedDeserOps.fromEither(chunkedInput.inputFor(0))
      constructorName <- ChunkedDeserOps.readOrGetConstructorName(input)
      result <- if (witness.value.name == constructorName) {
        ChunkedDeserOps.fromDeser(headCodec.value.deserialize(), input).map(headValue => Inl(field[K](headValue)))
      } else {
        tailCodec.deserialize().map(Inr.apply)
      }
    } yield result

  implicit def clistTransientDeserializer[K <: Symbol, H, T <: Coproduct](implicit witness: Witness.Aux[K],
                                                                          tailCodec: ChunkedBinaryDeserializer[T]): ChunkedBinaryDeserializer[FieldType[K, MarkedAsTransient[H]] :+: T] =
    () => tailCodec.deserialize().map(Inr.apply)

  def derive[T, H, Ks <: HList, Trs <: HList, Trcs <: HList, KsTrs <: HList, TH]
  (implicit gen: LabelledGeneric.Aux[T, H],
   keys: Lazy[Symbols.Aux[H, Ks]],
   transientAnnotations: Annotations.Aux[TransientField, T, Trs],
   transientConstructorAnnotations: Annotations.Aux[TransientConstructor, T, Trcs],
   taggedTransients: TagTransients.Aux[H, Trs, Trcs, TH],
   zip: Zip.Aux[Ks :: Trs :: HNil, KsTrs],
   toList: ToTraversable.Aux[KsTrs, List, (Symbol, Option[TransientField])],
   serializer: Lazy[ChunkedBinarySerializer[TH]],
   deserializer: Lazy[ChunkedBinaryDeserializer[TH]],
   toConstructorMap: Lazy[ToConstructorMap[TH]],
   classTag: ClassTag[T]): BinaryCodec[T] = {
    val constructorMap = toConstructorMap.value.constructors
    val constructorNameToId = constructorMap.zipWithIndex.toMap
    val constructorIdToName = constructorMap.zipWithIndex.map { case (name, id) => (id, name) }.toMap
    val trs = zip(keys.value() :: transientAnnotations() :: HNil)
    val transientFields = toList(trs).collect { case (key, Some(TransientField(defaultValue))) => (key, defaultValue) }.toMap

    BinaryCodec.define[T] {
      value =>
        for {
          _ <- writeByte(version)
          primaryOutput <- getOutput
          chunkedOutput = createChunkedOutput(primaryOutput)
          genericValue = gen.to(value)
          state <- getSerializerState
          typeRegistry <- getOutputTypeRegistry
          initialState = ChunkedSerState(
            state,
            typeRegistry,
            lastIndexPerChunk = Map.empty,
            fieldIndices = Map.empty,
            constructorNameToId,
            constructorIdToName,
            typeDescription = classTag.runtimeClass.getName,
            readConstructorName = None,
            transientFields
          )
          finalState <- Ser.fromEither(
            serializer.value.serialize(taggedTransients.tag(genericValue))
              .provideService(chunkedOutput)
              .getState
              .map(_._1)
              .either
              .runResult(initialState))
          _ <- chunkedOutput.writeEvolutionHeader(finalState.fieldIndices)
          _ <- chunkedOutput.writeOrderedChunks()
          _ <- setSerializerState(finalState.serializerState)
        } yield ()
    } {
      for {
        storedVersion <- readByte()
        primaryInput <- getInput
        chunkedInput <- createChunkedInput(primaryInput, storedVersion)
        state <- getDeserializerState
        typeRegistry <- getInputTypeRegistry
        initialState = ChunkedSerState(
          state,
          typeRegistry,
          lastIndexPerChunk = Map.empty,
          fieldIndices = Map.empty,
          constructorNameToId,
          constructorIdToName,
          typeDescription = classTag.runtimeClass.getName,
          readConstructorName = None,
          transientFields)
        result <- Deser.fromEither(deserializer.value.deserialize()
          .provideService(chunkedInput)
          .getState
          .either
          .runResult(initialState))
        (finalState, hlist) = result
        _ <- setDeserializerState(finalState.serializerState)
      } yield gen.from(taggedTransients.untag(hlist))
    }
  }
}

object GenericBinaryCodec {
  /**
   * A common static instance of the binary codec for all types not having any evolution steps
   */
  val simple = new GenericBinaryCodec(Vector(InitialVersion))

  trait TransientTag[T]
  type MarkedAsTransient[T] = T @@ TransientTag[T]


}
