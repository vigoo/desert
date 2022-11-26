package io.github.vigoo.desert.zioschema

import io.github.vigoo.desert.codecs.OptionBinaryCodec
import io.github.vigoo.desert.ziosupport.codecs._
import io.github.vigoo.desert._
import zio.schema.{Deriver, Schema, StandardType}
import zio.{Chunk, Unsafe}

import scala.annotation.tailrec

object DerivedBinaryCodec extends DerivedBinaryCodecVersionSpecific {
  override lazy val deriver = BinaryCodecDeriver().cached.autoAcceptSummoned

  private final case class EnumBuilderState(result: Any)

  private object EnumBuilderState {
    val initial: EnumBuilderState = EnumBuilderState(null)
  }

  private final case class RecordBuilderState(fields: List[Any]) {
    def storeField(value: Any): RecordBuilderState =
      this.copy(fields = value :: fields)

    def asChunk: Chunk[Any] = Chunk.fromIterable(fields).reverse
  }

  private object RecordBuilderState {
    val initial: RecordBuilderState = RecordBuilderState(List.empty)
  }

  // TODO: precalculate things outside of AdtCodec
  private final case class BinaryCodecDeriver() extends Deriver[BinaryCodec] {
    override def deriveRecord[A](
        record: Schema.Record[A],
        fields: => Chunk[Deriver.WrappedF[BinaryCodec, _]],
        summoned: => Option[BinaryCodec[A]]
    ): BinaryCodec[A] =
      new AdtCodec[A, RecordBuilderState](
        evolutionSteps = getEvolutionStepsFromAnnotation(record.annotations),
        typeName = record.id.name,
        constructors = Vector(record.id.name),
        transientFields = getTransientFields(record.fields),
        getSerializationCommands = (value: A) =>
          record.fields
            .zip(fields)
            .map { case (field, fieldCodec) =>
              AdtCodec.SerializationCommand.WriteField[Any](
                field.name,
                field.get(value),
                () => fieldCodec.unwrap.asInstanceOf[BinaryCodec[Any]]
              )
            }
            .toList,
        deserializationCommands = record.fields.zip(fields).toList.map { case (field, fieldInstance) =>
          fieldToDeserializationCommand(field, fieldInstance.unwrap)
        },
        initialBuilderState = RecordBuilderState.initial,
        materialize = builderState =>
          Unsafe
            .unsafe { implicit u =>
              record.construct(builderState.asChunk)
            }
            .left
            .map(msg => DeserializationFailure(msg, None))
      )

    override def deriveEnum[A](
        `enum`: Schema.Enum[A],
        cases: => Chunk[Deriver.WrappedF[BinaryCodec, _]],
        summoned: => Option[BinaryCodec[A]]
    ): BinaryCodec[A] =
      new AdtCodec[A, EnumBuilderState](
        evolutionSteps = getEvolutionStepsFromAnnotation(`enum`.annotations),
        typeName = `enum`.id.name,
        constructors = `enum`.cases.filterNot(_.annotations.contains(transientConstructor())).map(_.id).toVector,
        transientFields = Map.empty,
        getSerializationCommands = (value: A) =>
          `enum`.cases.zipWithIndex
            .find(_._1.asInstanceOf[Schema.Case[Any, Any]].deconstructOption(value).isDefined) match {
            case Some((matchingCase, index)) =>
              val isTransient = matchingCase.annotations.contains(transientConstructor())
              if (isTransient)
                List(AdtCodec.SerializationCommand.Fail(SerializingTransientConstructor(matchingCase.id)))
              else
                List(
                  AdtCodec.SerializationCommand.WriteConstructor(
                    constructorName = matchingCase.id,
                    value = matchingCase.asInstanceOf[Schema.Case[Any, Any]].deconstruct(value),
                    () => cases(index).unwrap.asInstanceOf[BinaryCodec[Any]]
                  )
                )

            case None => List.empty
          },
        deserializationCommands = `enum`.cases.zipWithIndex
          .filterNot { case (c, _) => isTransient(c) }
          .map { case (c, index) =>
            AdtCodec.DeserializationCommand.ReadConstructor(
              c.id,
              () => cases(index).unwrap.asInstanceOf[BinaryCodec[Any]],
              (value: Any, state: EnumBuilderState) => state.copy(result = value)
            )
          }
          .toList,
        initialBuilderState = EnumBuilderState.initial,
        materialize = builderState => Right(builderState.result.asInstanceOf[A])
      )

    override def derivePrimitive[A](st: StandardType[A], summoned: => Option[BinaryCodec[A]]): BinaryCodec[A] =
      (st match {
        case StandardType.UnitType           => codecs.unitCodec
        case StandardType.StringType         => codecs.stringCodec
        case StandardType.BoolType           => codecs.booleanCodec
        case StandardType.ByteType           => codecs.byteCodec
        case StandardType.ShortType          => codecs.shortCodec
        case StandardType.IntType            => codecs.intCodec
        case StandardType.LongType           => codecs.longCodec
        case StandardType.FloatType          => codecs.floatCodec
        case StandardType.DoubleType         => codecs.doubleCodec
        case StandardType.BinaryType         => byteChunkCodec
        case StandardType.CharType           => codecs.charCodec
        case StandardType.UUIDType           => codecs.uuidCodec
        case StandardType.BigDecimalType     => codecs.javaBigDecimalCodec
        case StandardType.BigIntegerType     => codecs.javaBigIntegerCodec
        case StandardType.DayOfWeekType      => codecs.dayOfWeekCodec
        case StandardType.MonthType          => codecs.monthCodec
        case StandardType.MonthDayType       => codecs.monthDayCodec
        case StandardType.PeriodType         => codecs.periodCodec
        case StandardType.YearType           => codecs.yearCodec
        case StandardType.YearMonthType      => codecs.yearMonthCodec
        case StandardType.ZoneIdType         => codecs.zoneIdCodec
        case StandardType.ZoneOffsetType     => codecs.zoneOffsetCodec
        case StandardType.DurationType       => codecs.durationCodec
        case StandardType.InstantType        => codecs.instantCodec
        case StandardType.LocalDateType      => codecs.localDateCodec
        case StandardType.LocalTimeType      => codecs.localTimeCodec
        case StandardType.LocalDateTimeType  => codecs.localDateTimeCodec
        case StandardType.OffsetTimeType     => codecs.offsetTimeCodec
        case StandardType.OffsetDateTimeType => codecs.offsetDateTimeCodec
        case StandardType.ZonedDateTimeType  => codecs.zonedDateTimeCodec
      }).asInstanceOf[BinaryCodec[A]]

    override def deriveOption[A](
        option: Schema.Optional[A],
        inner: => BinaryCodec[A],
        summoned: => Option[BinaryCodec[Option[A]]]
    ): BinaryCodec[Option[A]] =
      codecs.optionCodec(inner)

    override def deriveEither[A, B](
        either: Schema.Either[A, B],
        left: => BinaryCodec[A],
        right: => BinaryCodec[B],
        summoned: => Option[BinaryCodec[Either[A, B]]]
    ): BinaryCodec[Either[A, B]] =
      codecs.eitherCodec(left, right)

    override def deriveSequence[C[`2`], A](
        sequence: Schema.Sequence[C[A], A, _],
        inner: => BinaryCodec[A],
        summoned: => Option[BinaryCodec[C[A]]]
    ): BinaryCodec[C[A]] = {
      val baseCodec = chunkCodec[A](inner)
      BinaryCodec.from(
        baseCodec.contramap(sequence.toChunk),
        baseCodec.map(sequence.fromChunk)
      )
    }

    override def deriveSet[A](
        set: Schema.Set[A],
        inner: => BinaryCodec[A],
        summoned: => Option[BinaryCodec[Set[A]]]
    ): BinaryCodec[Set[A]] =
      codecs.setCodec(inner)

    override def deriveMap[K, V](
        map: Schema.Map[K, V],
        key: => BinaryCodec[K],
        value: => BinaryCodec[V],
        summoned: => Option[BinaryCodec[Map[K, V]]]
    ): BinaryCodec[Map[K, V]] =
      codecs.mapCodec(key, value)

    override def deriveTransformedRecord[A, B](
        record: Schema.Record[A],
        transform: Schema.Transform[A, B, _],
        fields: => Chunk[Deriver.WrappedF[BinaryCodec, _]],
        summoned: => Option[BinaryCodec[B]]
    ): BinaryCodec[B] =
      new AdtCodec[B, RecordBuilderState](
        evolutionSteps = getEvolutionStepsFromAnnotation(record.annotations),
        typeName = record.id.name,
        constructors = Vector(record.id.name),
        transientFields = getTransientFields(record.fields),
        getSerializationCommands = (value: B) =>
          transform.g(value) match {
            case Left(failure) =>
              List(AdtCodec.SerializationCommand.Fail(SerializationFailure(failure, None)))
            case Right(value)  =>
              record.fields
                .zip(fields)
                .map { case (field, fieldCodec) =>
                  AdtCodec.SerializationCommand.WriteField[Any](
                    field.name,
                    field.get(value),
                    () => fieldCodec.unwrap.asInstanceOf[BinaryCodec[Any]]
                  )
                }
                .toList
          },
        deserializationCommands = record.fields.zip(fields).toList.map { case (field, fieldInstance) =>
          fieldToDeserializationCommand(field, fieldInstance.unwrap)
        },
        initialBuilderState = RecordBuilderState.initial,
        materialize = builderState =>
          Unsafe
            .unsafe { implicit u =>
              record.construct(builderState.asChunk)
            }
            .flatMap { a =>
              transform.f(a)
            }
            .left
            .map(msg => DeserializationFailure(msg, None))
      )

    override def deriveTupleN[T](
        schemasAndInstances: => Chunk[(Schema[_], Deriver.WrappedF[BinaryCodec, _])],
        summoned: => Option[BinaryCodec[T]]
    ): BinaryCodec[T] = {
      val (schemas, wrappedInstances) = schemasAndInstances.unzip
      val instances                   = wrappedInstances.map(_.unwrap.asInstanceOf[BinaryCodec[Any]])
      schemasAndInstances.length match {
        case 2 =>
          codecs
            .tuple2Codec(instances(0), fieldReaderFor(instances(0)), instances(1), fieldReaderFor(instances(1)))
            .asInstanceOf[BinaryCodec[T]]
      }
    }

    private def fieldReaderFor(instance: BinaryCodec[_]): codecs.TupleFieldReader[Any] =
      instance match {
        case optional: OptionBinaryCodec[_] =>
          codecs.TupleFieldReader
            .optionalFieldReader[Any](optional.asInstanceOf[BinaryCodec[Any]])
            .asInstanceOf[codecs.TupleFieldReader[Any]]
        case _                              =>
          codecs.TupleFieldReader.requiredFieldReader[Any](instance.asInstanceOf[BinaryCodec[Any]])
      }

    private def getEvolutionStepsFromAnnotation(value: Chunk[Any]): Vector[Evolution] =
      value.collectFirst { case ann: evolutionSteps => ann } match {
        case None             => Vector(InitialVersion)
        case Some(annotation) => InitialVersion +: annotation.steps.toVector
      }

    private def isTransient(c: Schema.Case[_, _]): Boolean =
      c.annotations.contains(transientConstructor())

    private def getTransientFields(fields: Chunk[Schema.Field[_, _]]): Map[Symbol, Any] =
      fields.flatMap { field =>
        field.annotations.collect { case transientField(defaultValue) =>
          Symbol(field.name) -> defaultValue
        }
      }.toMap

    @tailrec
    private[zioschema] final def findTopLevelOptionalNode(value: Schema[_]): Option[Schema.Optional[_]] =
      value match {
        case _: Schema.Enum[_]                                    => None
        case _: Schema.Record[_]                                  => None
        case _: Schema.Collection[_, _]                           => None
        case Schema.Transform(codec, f, g, annotations, identity) =>
          findTopLevelOptionalNode(codec)
        case Schema.Primitive(standardType, annotations)          => None
        case s @ Schema.Optional(codec, annotations)              => Some(s)
        case Schema.Fail(message, annotations)                    => None
        case Schema.Tuple2(left, right, annotations)              => None
        case Schema.Either(left, right, annotations)              => None
        case Schema.Lazy(schema0)                                 => findTopLevelOptionalNode(schema0())
        case Schema.Dynamic(annotations)                          => None
      }

    private def fieldToDeserializationCommand(
        field: Schema.Field[_, _],
        instance: => BinaryCodec[_]
    ): AdtCodec.DeserializationCommand[RecordBuilderState] = {
      val optional            = findTopLevelOptionalNode(field.schema)
      val transientAnnotation = field.annotations.collectFirst { case tf: transientField =>
        tf
      }

      if (transientAnnotation.isDefined)
        AdtCodec.DeserializationCommand.ReadTransient(
          field.name,
          (value: Any, state: RecordBuilderState) => state.storeField(value)
        )
      else if (optional.isDefined) {
        val optionCodec = instance.asInstanceOf[OptionBinaryCodec[Any]]
        AdtCodec.DeserializationCommand.ReadOptional(
          field.name,
          () => optionCodec.innerCodec,
          () => optionCodec,
          (value: Option[Any], state: RecordBuilderState) => state.storeField(value)
        )
      } else
        AdtCodec.DeserializationCommand.Read(
          field.name,
          () => instance.asInstanceOf[BinaryCodec[Any]],
          (value: Any, state: RecordBuilderState) => state.storeField(value)
        )
    }
  }
}
