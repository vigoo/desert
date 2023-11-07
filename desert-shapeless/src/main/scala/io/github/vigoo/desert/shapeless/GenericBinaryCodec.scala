package io.github.vigoo.desert.shapeless

import _root_.shapeless._
import _root_.shapeless.labelled._
import _root_.shapeless.ops.hlist._
import _root_.shapeless.tag._
import io.github.vigoo.desert._
import io.github.vigoo.desert.internal.AdtCodec
import io.github.vigoo.desert.shapeless.GenericBinaryCodec._

import scala.collection.immutable.ListMap
import scala.reflect.ClassTag

/** Trait containing the more generic implicits backing up the generic binary codec
  *
  * These are the default implementations while in [[GenericDerivationApi]] there are some more specialized ones for
  * handling optional and transient fields / constructors.
  */
trait LowerPriorityGenericDerivationApi {
  implicit def hlistSerializer[K <: Symbol, H, T <: HList](implicit
      witness: Witness.Aux[K],
      headCodec: Lazy[BinaryCodec[H]],
      tailPlan: SerializationPlan[T]
  ): SerializationPlan[FieldType[K, H] :: T]

  implicit def hlistDeserializer[K <: Symbol, H, T <: HList](implicit
      witness: Witness.Aux[K],
      headCodec: Lazy[BinaryCodec[H]],
      tailPlan: DeserializationPlan[T]
  ): DeserializationPlan[FieldType[K, H] :: T]

  implicit def clistSerializer[K <: Symbol, H, T <: Coproduct](implicit
      witness: Witness.Aux[K],
      headCodec: Lazy[BinaryCodec[H]],
      tailPlan: SerializationPlan[T]
  ): SerializationPlan[FieldType[K, H] :+: T]

  implicit def clistDeserializer[K <: Symbol, H, T <: Coproduct](implicit
      witness: Witness.Aux[K],
      headCodec: Lazy[BinaryCodec[H]],
      tailPlan: DeserializationPlan[T]
  ): DeserializationPlan[FieldType[K, H] :+: T]
}

/** Trait containing the public interface for the generic binary codec.
  *
  * As there is no single global generic derivation implementation because it is parameteric with the evolution steps, a
  * specific type's codec derviation is possible by accessing the necessary implicits (for HLists and Coproducts) via
  * this interface.
  *
  * The two type classes implemented for both products and coproducts are [[SerializationPlan]] and
  * [[DeserializationPlan]]. These monads are more specific than the [[BinarySerializer]] and [[BinaryDeserializer]],
  * holding state specific for a single evolvable value's serialization.
  */
trait GenericDerivationApi extends LowerPriorityGenericDerivationApi {
  implicit val hnilSerializer: SerializationPlan[HNil]
  implicit val hnilDeserializer: DeserializationPlan[HNil]
  implicit val cnilSerializer: SerializationPlan[CNil]
  implicit val cnilDeserializer: DeserializationPlan[CNil]

  implicit def hlistTransientSerializer[K <: Symbol, H, T <: HList](implicit
      witness: Witness.Aux[K],
      tailPlan: SerializationPlan[T]
  ): SerializationPlan[FieldType[K, MarkedAsTransient[H]] :: T]

  implicit def hlistOptionalDeserializer[K <: Symbol, H, T <: HList](implicit
      witness: Witness.Aux[K],
      headCodec: Lazy[BinaryCodec[H]],
      optHeadCodec: Lazy[BinaryCodec[Option[H]]],
      tailPlan: DeserializationPlan[T]
  ): DeserializationPlan[FieldType[K, Option[H]] :: T]

  implicit def hlistTransientDeserializer[K <: Symbol, H, T <: HList](implicit
      witness: Witness.Aux[K],
      tailPlan: DeserializationPlan[T]
  ): DeserializationPlan[FieldType[K, MarkedAsTransient[H]] :: T]

  implicit def clistTransientSerializer[K <: Symbol, H, T <: Coproduct](implicit
      witness: Witness.Aux[K],
      tailPlan: SerializationPlan[T]
  ): SerializationPlan[FieldType[K, MarkedAsTransient[H]] :+: T]

  implicit def clistTransientDeserializer[K <: Symbol, H, T <: Coproduct](implicit
      witness: Witness.Aux[K],
      tailPlan: DeserializationPlan[T]
  ): DeserializationPlan[FieldType[K, MarkedAsTransient[H]] :+: T]

  /** Type class to tag a labelled generic representation of a type with a type level information about whether each
    * field or constructor is marked as transient or not.
    *
    * For products 'H' is a [[HList]], for coproducts it is a [[Coproduct]], with each item being tagged with the field
    * or constructor's name by [[LabelledGeneric]], having a type of 'FieldType[K, V]'.
    *
    * The result of this type-level operation, 'Result' is a [[HList]] or [[Coproduct]] where those fields or
    * constructors that are having a transient attribute of [[transientField]] or [[transientConstructor]] are also
    * tagged with the [[MarkedAsTransient]] tag, so each element is either 'FieldType[K, V]' or 'FieldType[K,
    * MarkedAsTransient[V]]'.
    *
    * This makes it possible to differentiate in compile time between transient and non-transient flags in the
    * implementation of the [[DeserializationPlan]] and [[SerializationPlan]] type classes.
    *
    * @tparam H
    *   The generic representation of the type
    * @tparam AF
    *   [[HList]] of the [[transientField]] annotations, extracted by [[Annotations]]
    * @tparam AC
    *   [[HList]] of the [[transientConstructor]] annotations, extracted by [[Annotations]]
    */
  trait TagTransients[H, AF, AC] {
    type Result

    /** Create a representation of the labelled generic value that also has transient tags
      */
    def tag(value: H): Result

    /** Removes the transient tags from a labelled generic representation
      */
    def untag(value: Result): H
  }

  /** Implementation of [[TagTransients]] for both products and coproducts
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

    implicit def prodTagTransientsTransient[K <: Symbol, H, T <: HList, TT <: HList, AT <: HList, AC](implicit
        tailTagger: TagTransients.Aux[T, AT, AC, TT]
    ): TagTransients.Aux[FieldType[K, H] :: T, Some[transientField] :: AT, AC, FieldType[
      K,
      MarkedAsTransient[H]
    ] :: TT] =
      new TagTransients[FieldType[K, H] :: T, Some[transientField] :: AT, AC] {
        override type Result = FieldType[K, MarkedAsTransient[H]] :: TT
        override def tag(value: FieldType[K, H] :: T): Result                                     = value match {
          case head :: tail =>
            head.asInstanceOf[FieldType[K, MarkedAsTransient[H]]] :: tailTagger.tag(tail)
        }
        override def untag(value: FieldType[K, MarkedAsTransient[H]] :: TT): FieldType[K, H] :: T = value match {
          case head :: tail =>
            head.asInstanceOf[FieldType[K, H]] :: tailTagger.untag(tail)
        }
      }

    implicit def prodTagTransientsPersistent[K <: Symbol, H, T <: HList, TT <: HList, AT <: HList, AC](implicit
        tailTagger: TagTransients.Aux[T, AT, AC, TT]
    ): TagTransients.Aux[FieldType[K, H] :: T, None.type :: AT, AC, FieldType[K, H] :: TT] =
      new TagTransients[FieldType[K, H] :: T, None.type :: AT, AC] {
        override type Result = FieldType[K, H] :: TT
        override def tag(value: FieldType[K, H] :: T): Result                  = value match {
          case head :: tail =>
            head :: tailTagger.tag(tail)
        }
        override def untag(value: FieldType[K, H] :: TT): FieldType[K, H] :: T = value match {
          case head :: tail =>
            head :: tailTagger.untag(tail)
        }
      }

    implicit def coprodTagTransientsPersistent[K <: Symbol, H, T <: Coproduct, TT <: Coproduct, AT, AC <: HList](
        implicit tailTagger: TagTransients.Aux[T, AT, AC, TT]
    ): TagTransients.Aux[FieldType[K, H] :+: T, AT, None.type :: AC, FieldType[K, H] :+: TT] =
      new TagTransients[FieldType[K, H] :+: T, AT, None.type :: AC] {
        override type Result = FieldType[K, H] :+: TT
        override def tag(value: FieldType[K, H] :+: T): Result                   = value match {
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

    implicit def coprodTagTransientsTransient[K <: Symbol, H, T <: Coproduct, TT <: Coproduct, AT, AC <: HList](implicit
        tailTagger: TagTransients.Aux[T, AT, AC, TT]
    ): TagTransients.Aux[FieldType[K, H] :+: T, AT, Some[transientConstructor] :: AC, FieldType[K, MarkedAsTransient[
      H
    ]] :+: TT] =
      new TagTransients[FieldType[K, H] :+: T, AT, Some[transientConstructor] :: AC] {
        override type Result = FieldType[K, MarkedAsTransient[H]] :+: TT
        override def tag(value: FieldType[K, H] :+: T): Result                                      = value match {
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

  /** Type class extracting the field names or constructor names of a labelled generic representation to a [[HList]] of
    * [[Symbol]] values
    * @tparam H
    *   The [[LabelledGeneric]] representation of a type
    */
  trait Symbols[H] {
    type Result <: HList

    /** Extracts the [[HList]] of [[Symbol]] values from the type-level labelled generic representation
      */
    def apply(): Result
  }

  /** Implementation of the [[Symbols]] type class for both products and coproducts
    */
  object Symbols {
    type Aux[H, R <: HList] = Symbols[H] { type Result = R }

    implicit val hnilSymbols: Symbols.Aux[HNil, HNil] = new Symbols[HNil] {
      override type Result = HNil
      override def apply(): Result = HNil
    }

    implicit def hlistSymbols[K <: Symbol, H, T <: HList, TR <: HList](implicit
        witness: Witness.Aux[K],
        tailSymbols: Symbols.Aux[T, TR]
    ): Symbols.Aux[FieldType[K, H] :: T, Symbol :: TR] =
      new Symbols[FieldType[K, H] :: T] {
        override type Result = Symbol :: TR
        override def apply(): Result = witness.value :: tailSymbols()
      }

    implicit val cnilSymbols: Symbols.Aux[CNil, HNil] = new Symbols[CNil] {
      override type Result = HNil
      override def apply(): Result = HNil
    }

    implicit def clistSymbols[K <: Symbol, H, T <: Coproduct, TR <: HList](implicit
        witness: Witness.Aux[K],
        tailSymbols: Symbols.Aux[T, TR]
    ): Symbols.Aux[FieldType[K, H] :+: T, Symbol :: TR] =
      new Symbols[FieldType[K, H] :+: T] {
        override type Result = Symbol :: TR
        override def apply(): Result = witness.value :: tailSymbols()
      }
  }

  /** Type class to extract the type-level generic constructor names to a run-time vector of strings
    * @tparam T
    *   The [[LabelledGeneric]] representation of a type tagged by [[TagTransients]]
    */
  trait ToConstructorMap[T] {
    val constructors: Vector[String]
  }

  /** Lower priority implicits for the implementation of [[ToConstructorMap]].
    *
    * This enables capturing the [[MarkedAsTransient]] tagged constructors in the [[ToConstructorMap]]
    */
  trait ToConstructorMapLowPriority {
    implicit def clist[K <: Symbol, H, T <: Coproduct](implicit
        witness: Witness.Aux[K],
        tail: ToConstructorMap[T]
    ): ToConstructorMap[FieldType[K, H] :+: T] =
      new ToConstructorMap[FieldType[K, H] :+: T] {
        val constructors: Vector[String] = witness.value.name +: tail.constructors
      }
  }

  /** Implementation of the [[ToConstructorMap]] type class for both products and coproducts.
    *
    * For products the result is always an empty vector. For coproducts the result if the vector of constructor names,
    * except the ones marked as transient.
    */
  object ToConstructorMap extends ToConstructorMapLowPriority {
    implicit val cnil: ToConstructorMap[CNil]           = new ToConstructorMap[CNil] {
      val constructors: Vector[String] = Vector.empty
    }
    implicit def clistTransient[K <: Symbol, H, T <: Coproduct](implicit
        witness: Witness.Aux[K],
        tail: ToConstructorMap[T]
    ): ToConstructorMap[FieldType[K, MarkedAsTransient[H]] :+: T] =
      new ToConstructorMap[FieldType[K, MarkedAsTransient[H]] :+: T] {
        val constructors: Vector[String] = tail.constructors
      }
    implicit val hnil: ToConstructorMap[HNil]           = new ToConstructorMap[HNil] {
      val constructors: Vector[String] = Vector.empty
    }
    implicit def hlist[H <: HList]: ToConstructorMap[H] = new ToConstructorMap[H] {
      val constructors: Vector[String] = Vector.empty
    }
  }

  /** The entry point of deriving a generic binary codec for a type 'T'
    *
    * The derivation needs both type level and runtime information about the type it derives the codec for. On type
    * level first the [[LabelledGeneric]] representation gets extracted into 'H'. This is either a [[HList]] of fields
    * or a [[Coproduct]] of constructors. Then with [[Annotations]] it finds both the [[transientField]] and
    * [[transientConstructor]] annotations for each field and constructor, stored in the 'Trs' and 'Trcs' types. Based
    * on these the [[TagTransients]] type level operation constructs 'TH' where each field/constructor is tagged with
    * its label and optionally as transient. This is the type which is then used recursively to generate the serializer
    * and the deserializer through the implicits of [[SerializationPlan]] and [[DeserializationPlan]].
    *
    * For these serializer and deserializer operations we also need some runtime data. The [[ToConstructorMap]] type
    * class extracts the name of non-transient constructors into a runtime [[Vector]]. This defines the association
    * between constructors and numeric IDs.
    *
    * It is not enough to have transient tags in type level because the [[transientField]] annotation also holds a
    * default value. For this reason we need to be able to access the default value runtime as well, during the
    * construction of the generic representation in the deserializer.
    *
    * With 'keys', 'zip' and 'toList' we zip together the field names with the annotations and convert the result to a
    * runtime [[Map]].
    *
    * @param gen
    *   Extracts the labelled generic representation of type 'T' into 'H'
    * @param keys
    *   Gets the HList of field/constructor names of 'H' into 'Ks'
    * @param transientAnnotations
    *   Extracts the [[transientField]] annotations of 'T' into 'Trs'
    * @param transientConstructorAnnotations
    *   Extracts the [[transientConstructor]] annotations of 'T' into 'Trcs'
    * @param taggedTransients
    *   Tags the generic representation 'H' with transient tags based on 'Trs' and 'Trcs' into 'TH'
    * @param zip
    *   Creates a zipped list of 'Ks' and 'Trs', associating field names with transient annotations into 'KsTrs'
    * @param toList
    *   Extractor of the 'KsTrs' [[HList]] into a [[List]] of [[Symbol]] and optional [[transientField]] pairs
    * @param serializationPlan
    *   The serializer implementation for the tagged generic representation 'TH'
    * @param deserializationPlan
    *   The deserializer implementation for the tagged generic representation 'TH'
    * @param toConstructorMap
    *   Extracts the names of the constructors of a coproduct from the generic representation 'TH' into a runtime vector
    * @param classTag
    *   Class tag for the type 'T'
    * @tparam T
    *   Type to derive the generic representation for
    * @tparam H
    *   Labelled generic representation of type 'T'
    * @tparam Ks
    *   [[HList]] of field/constructor names
    * @tparam Trs
    *   [[HList]] of optional [[transientField]] annotations per field/constructor
    * @tparam Trcs
    *   [[HList]] of optional [[transientConstructor]] annotations per field/constructor
    * @tparam KsTrs
    *   [[HList]] of pairs of [[Symbol]] and optional [[transientField]] values
    * @tparam TH
    *   Tagged generic representation of type 'T' produced by [[TagTransients]]
    * @return
    *   The derived [[BinaryCodec]] for type 'T'
    */
  def derive[T, H, Ks <: HList, Trs <: HList, Trcs <: HList, KsTrs <: HList, TH](implicit
      gen: LabelledGeneric.Aux[T, H],
      keys: Lazy[Symbols.Aux[H, Ks]],
      transientAnnotations: Annotations.Aux[transientField, T, Trs],
      transientConstructorAnnotations: Annotations.Aux[transientConstructor, T, Trcs],
      taggedTransients: TagTransients.Aux[H, Trs, Trcs, TH],
      zip: Zip.Aux[Ks :: Trs :: HNil, KsTrs],
      toList: ToTraversable.Aux[KsTrs, List, (Symbol, Option[transientField])],
      serializationPlan: Lazy[SerializationPlan[TH]],
      deserializationPlan: Lazy[DeserializationPlan[TH]],
      toConstructorMap: Lazy[ToConstructorMap[TH]],
      classTag: ClassTag[T]
  ): BinaryCodec[T]
}

class GenericBinaryCodec(evolutionSteps: Vector[Evolution]) extends GenericDerivationApi {
  implicit val hnilSerializer: SerializationPlan[HNil] =
    new SerializationPlan[HNil] {
      override def commands(value: HNil): List[AdtCodec.SerializationCommand] = List.empty
    }

  implicit val hnilDeserializer: DeserializationPlan[HNil] =
    new DeserializationPlan[HNil] {
      override val commands: List[AdtCodec.DeserializationCommand[BuilderState]] = List.empty

      override def materialize(values: BuilderState): HNil = HNil
    }

  implicit val cnilSerializer: SerializationPlan[CNil]     =
    new SerializationPlan[CNil] {
      override def commands(value: CNil): List[AdtCodec.SerializationCommand] = List.empty
    }
  implicit val cnilDeserializer: DeserializationPlan[CNil] =
    new DeserializationPlan[CNil] {
      override val commands: List[AdtCodec.DeserializationCommand[BuilderState]] = List.empty

      override def materialize(values: BuilderState): CNil = ???
    }

  implicit def hlistTransientSerializer[K <: Symbol, H, T <: HList](implicit
      witness: Witness.Aux[K],
      tailPlan: SerializationPlan[T]
  ): SerializationPlan[FieldType[K, MarkedAsTransient[H]] :: T] =
    new SerializationPlan[FieldType[K, MarkedAsTransient[H]] :: T] {
      override def commands(value: FieldType[K, MarkedAsTransient[H]] :: T): List[AdtCodec.SerializationCommand] =
        value match {
          case _ :: tailValues => tailPlan.commands(tailValues)
        }
    }

  implicit def hlistSerializer[K <: Symbol, H, T <: HList](implicit
      witness: Witness.Aux[K],
      headCodec: Lazy[BinaryCodec[H]],
      tailPlan: SerializationPlan[T]
  ): SerializationPlan[FieldType[K, H] :: T] =
    new SerializationPlan[FieldType[K, H] :: T] {
      override def commands(value: FieldType[K, H] :: T): List[AdtCodec.SerializationCommand] =
        value match {
          case headValue :: tailValues =>
            AdtCodec.SerializationCommand.WriteField[H](witness.value.name, headValue, () => headCodec.value) ::
              tailPlan.commands(tailValues)
        }
    }

  implicit def hlistTransientDeserializer[K <: Symbol, H, T <: HList](implicit
      witness: Witness.Aux[K],
      tailPlan: DeserializationPlan[T]
  ): DeserializationPlan[FieldType[K, MarkedAsTransient[H]] :: T] =
    new DeserializationPlan[FieldType[K, MarkedAsTransient[H]] :: T] {
      override def commands: List[AdtCodec.DeserializationCommand[BuilderState]] =
        AdtCodec.DeserializationCommand.ReadTransient(
          witness.value.name,
          (value: H, values: BuilderState) => values.storeFieldValue(witness.value.name, value)
        ) :: tailPlan.commands

      override def materialize(values: BuilderState): FieldType[K, MarkedAsTransient[H]] :: T =
        field[K](values.getField[MarkedAsTransient[H]](witness.value.name)) :: tailPlan.materialize(values)
    }

  implicit def hlistOptionalDeserializer[K <: Symbol, H, T <: HList](implicit
      witness: Witness.Aux[K],
      headCodec: Lazy[BinaryCodec[H]],
      optHeadCodec: Lazy[BinaryCodec[Option[H]]],
      tailPlan: DeserializationPlan[T]
  ): DeserializationPlan[FieldType[K, Option[H]] :: T] =
    new DeserializationPlan[FieldType[K, Option[H]] :: T] {
      override def commands: List[AdtCodec.DeserializationCommand[BuilderState]] =
        AdtCodec.DeserializationCommand.ReadOptional(
          witness.value.name,
          () => headCodec.value,
          () => optHeadCodec.value,
          (value: Option[H], values: BuilderState) => values.storeFieldValue(witness.value.name, value)
        ) :: tailPlan.commands

      override def materialize(values: BuilderState): FieldType[K, Option[H]] :: T =
        field[K](values.getField[Option[H]](witness.value.name)) :: tailPlan.materialize(values)
    }

  implicit def hlistDeserializer[K <: Symbol, H, T <: HList](implicit
      witness: Witness.Aux[K],
      headCodec: Lazy[BinaryCodec[H]],
      tailPlan: DeserializationPlan[T]
  ): DeserializationPlan[FieldType[K, H] :: T] =
    new DeserializationPlan[FieldType[K, H] :: T] {
      override def commands: List[AdtCodec.DeserializationCommand[BuilderState]] =
        AdtCodec.DeserializationCommand.Read(
          witness.value.name,
          () => headCodec.value,
          (value: H, values: BuilderState) => values.storeFieldValue(witness.value.name, value)
        ) :: tailPlan.commands

      override def materialize(values: BuilderState): FieldType[K, H] :: T =
        field[K](values.getField[H](witness.value.name)) :: tailPlan.materialize(values)
    }

  implicit def clistSerializer[K <: Symbol, H, T <: Coproduct](implicit
      witness: Witness.Aux[K],
      headCodec: Lazy[BinaryCodec[H]],
      tailPlan: SerializationPlan[T]
  ): SerializationPlan[FieldType[K, H] :+: T] =
    new SerializationPlan[FieldType[K, H] :+: T] {
      override def commands(value: FieldType[K, H] :+: T): List[AdtCodec.SerializationCommand] =
        value match {
          case Inl(headValue) =>
            AdtCodec.SerializationCommand.WriteConstructor(
              witness.value.name,
              headValue,
              () => headCodec.value
            ) :: List.empty
          case Inr(tail)      =>
            tailPlan.commands(tail)
        }
    }

  implicit def clistTransientSerializer[K <: Symbol, H, T <: Coproduct](implicit
      witness: Witness.Aux[K],
      tailPlan: SerializationPlan[T]
  ): SerializationPlan[FieldType[K, MarkedAsTransient[H]] :+: T] =
    new SerializationPlan[FieldType[K, MarkedAsTransient[H]] :+: T] {
      override def commands(value: FieldType[K, MarkedAsTransient[H]] :+: T): List[AdtCodec.SerializationCommand] =
        value match {
          case Inl(_)    =>
            AdtCodec.SerializationCommand.Fail(
              DesertFailure.SerializingTransientConstructor(witness.value.name)
            ) :: List.empty
          case Inr(tail) =>
            tailPlan.commands(tail)
        }
    }

  implicit def clistDeserializer[K <: Symbol, H, T <: Coproduct](implicit
      witness: Witness.Aux[K],
      headCodec: Lazy[BinaryCodec[H]],
      tailPlan: DeserializationPlan[T]
  ): DeserializationPlan[FieldType[K, H] :+: T] =
    new DeserializationPlan[FieldType[K, H] :+: T] {
      override def commands: List[AdtCodec.DeserializationCommand[BuilderState]] =
        AdtCodec.DeserializationCommand.ReadConstructor(
          witness.value.name,
          () => headCodec.value,
          (value: H, values: BuilderState) => values.storeSelectedConstructor(witness.value.name, value)
        ) ::
          tailPlan.commands

      override def materialize(values: BuilderState): FieldType[K, H] :+: T =
        if (values.selectedConstructor == witness.value.name)
          Inl(field[K](values.selectedConstructorValue.asInstanceOf[H]))
        else
          Inr(tailPlan.materialize(values))
    }

  implicit def clistTransientDeserializer[K <: Symbol, H, T <: Coproduct](implicit
      witness: Witness.Aux[K],
      tailPlan: DeserializationPlan[T]
  ): DeserializationPlan[FieldType[K, MarkedAsTransient[H]] :+: T] =
    new DeserializationPlan[FieldType[K, MarkedAsTransient[H]] :+: T] {
      override def commands: List[AdtCodec.DeserializationCommand[BuilderState]] =
        tailPlan.commands

      override def materialize(values: BuilderState): FieldType[K, MarkedAsTransient[H]] :+: T =
        Inr(tailPlan.materialize(values))
    }

  def derive[T, H, Ks <: HList, Trs <: HList, Trcs <: HList, KsTrs <: HList, TH](implicit
      gen: LabelledGeneric.Aux[T, H],
      keys: Lazy[Symbols.Aux[H, Ks]],
      transientAnnotations: Annotations.Aux[transientField, T, Trs],
      transientConstructorAnnotations: Annotations.Aux[transientConstructor, T, Trcs],
      taggedTransients: TagTransients.Aux[H, Trs, Trcs, TH],
      zip: Zip.Aux[Ks :: Trs :: HNil, KsTrs],
      toList: ToTraversable.Aux[KsTrs, List, (Symbol, Option[transientField])],
      serializationPlan: Lazy[SerializationPlan[TH]],
      deserializationPlan: Lazy[DeserializationPlan[TH]],
      toConstructorMap: Lazy[ToConstructorMap[TH]],
      classTag: ClassTag[T]
  ): BinaryCodec[T] = {
    val constructorMap  = toConstructorMap.value.constructors
    val trs             = zip(keys.value() :: transientAnnotations() :: HNil)
    val transientFields = toList(trs).collect { case (key, Some(transientField(defaultValue))) =>
      (key.name, defaultValue)
    }.toMap

    new AdtCodec[T, BuilderState](
      evolutionSteps,
      classTag.runtimeClass.getName,
      constructorMap,
      transientFields,
      (value: T) => {
        val genericValue = gen.to(value)
        serializationPlan.value.commands(taggedTransients.tag(genericValue))
      },
      deserializationPlan.value.commands,
      () => BuilderState.empty,
      (values => Right(gen.from(taggedTransients.untag(deserializationPlan.value.materialize(values)))))
    )
  }

}

object GenericBinaryCodec {

  /** A common static instance of the binary codec for all types not having any evolution steps
    */
  val simple = new GenericBinaryCodec(Vector(Evolution.InitialVersion))

  final case class BuilderState(
      values: ListMap[String, Any],
      selectedConstructor: String,
      selectedConstructorValue: Any
  ) {
    def storeFieldValue[T](fieldName: String, value: T): BuilderState =
      this.copy(values = values.updated(fieldName, value))

    def storeSelectedConstructor[T](constructorName: String, value: T): BuilderState =
      this.copy(
        selectedConstructor = constructorName,
        selectedConstructorValue = value
      )

    def getField[T](fieldName: String): T = values(fieldName).asInstanceOf[T]
  }

  object BuilderState {
    val empty: BuilderState = BuilderState(ListMap.empty, null, null)
  }

  trait TransientTag[T]
  type MarkedAsTransient[T] = T @@ TransientTag[T]

  trait SerializationPlan[T] {
    def commands(value: T): List[AdtCodec.SerializationCommand]
  }

  trait DeserializationPlan[T] {
    def commands: List[AdtCodec.DeserializationCommand[BuilderState]]
    def materialize(values: BuilderState): T
  }
}
