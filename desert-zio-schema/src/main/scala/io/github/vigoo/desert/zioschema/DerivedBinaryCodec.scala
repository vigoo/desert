package io.github.vigoo.desert.zioschema

import io.github.vigoo.desert.ziosupport._
import io.github.vigoo.desert._
import io.github.vigoo.desert.internal.{AdtCodec, OptionBinaryCodec}
import zio.schema.{Deriver, Schema, StandardType}
import zio.{Chunk, ChunkBuilder, Unsafe}

import scala.annotation.tailrec

object DerivedBinaryCodec extends DerivedBinaryCodecVersionSpecific {
  override lazy val deriver = BinaryCodecDeriver().cached.autoAcceptSummoned

  private final case class EnumBuilderState(result: Any)

  private object EnumBuilderState {
    val initial: EnumBuilderState = EnumBuilderState(null)
  }

  private final case class RecordBuilderState(fields: ChunkBuilder[Any]) {
    def storeField(value: Any): RecordBuilderState = {
      fields.addOne(value)
      this
    }

    def asChunk: Chunk[Any] = fields.result()
  }

  private object RecordBuilderState {
    def initial(fieldCount: Int): RecordBuilderState = RecordBuilderState(ChunkBuilder.make(fieldCount))
  }

  private final case class BinaryCodecDeriver() extends Deriver[BinaryCodec] {
    override def deriveRecord[A](
        record: Schema.Record[A],
        fields: => Chunk[Deriver.WrappedF[BinaryCodec, _]],
        summoned: => Option[BinaryCodec[A]]
    ): BinaryCodec[A] = {
      val transientFields               = getTransientFields(record.fields)
      val preparedSerializationCommands =
        record.fields.zipWithIndex
          .filter { case (field, _) => !transientFields.contains(field.name) }
          .map { case (field, idx) =>
            val fieldCodec = fields(idx)
            (field.name, field.get, () => fieldCodec.unwrap.asInstanceOf[BinaryCodec[Any]])
          }
          .toList

      Unsafe
        .unsafe { implicit u =>
          new AdtCodec[A, RecordBuilderState](
            evolutionSteps = getEvolutionStepsFromAnnotation(record.annotations),
            typeName = record.id.name,
            constructors = Vector(record.id.name),
            transientFields = transientFields,
            getSerializationCommands = (value: A) =>
              preparedSerializationCommands.map { case (fieldName, getter, codec) =>
                AdtCodec.SerializationCommand.WriteField[Any](
                  fieldName,
                  getter(value),
                  codec
                )
              },
            deserializationCommands = record.fields.zip(fields).toList.map { case (field, fieldInstance) =>
              fieldToDeserializationCommand(field, fieldInstance.unwrap)
            },
            initialBuilderState = () => RecordBuilderState.initial(record.fields.size),
            materialize = builderState =>
              record
                .construct(builderState.asChunk)
                .left
                .map(msg => DesertFailure.DeserializationFailure(msg, None))
          )
        }
    }

    override def deriveEnum[A](
        `enum`: Schema.Enum[A],
        cases: => Chunk[Deriver.WrappedF[BinaryCodec, _]],
        summoned: => Option[BinaryCodec[A]]
    ): BinaryCodec[A] = {
      val indexedCases       = `enum`.cases.zipWithIndex
      val isTransientPerCase = `enum`.cases.map { enumCase =>
        enumCase.annotations.exists(_.isInstanceOf[transientConstructor])
      }

      new AdtCodec[A, EnumBuilderState](
        evolutionSteps = getEvolutionStepsFromAnnotation(`enum`.annotations),
        typeName = `enum`.id.name,
        constructors = `enum`.cases.filterNot(_.annotations.contains(transientConstructor())).map(_.id).toVector,
        transientFields = Map.empty,
        getSerializationCommands = (value: A) =>
          indexedCases
            .find(_._1.asInstanceOf[Schema.Case[Any, Any]].deconstructOption(value).isDefined) match {
            case Some((matchingCase, index)) =>
              val isTransient = isTransientPerCase(index)
              if (isTransient)
                List(AdtCodec.SerializationCommand.Fail(DesertFailure.SerializingTransientConstructor(matchingCase.id)))
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
        deserializationCommands = indexedCases
          .filterNot { case (c, _) => isTransient(c) }
          .map { case (c, index) =>
            AdtCodec.DeserializationCommand.ReadConstructor(
              c.id,
              () => cases(index).unwrap.asInstanceOf[BinaryCodec[Any]],
              (value: Any, state: EnumBuilderState) => state.copy(result = value)
            )
          }
          .toList,
        initialBuilderState = () => EnumBuilderState.initial,
        materialize = builderState => Right(builderState.result.asInstanceOf[A])
      )
    }

    override def derivePrimitive[A](st: StandardType[A], summoned: => Option[BinaryCodec[A]]): BinaryCodec[A] =
      (st match {
        case StandardType.UnitType           => unitCodec
        case StandardType.StringType         => stringCodec
        case StandardType.BoolType           => booleanCodec
        case StandardType.ByteType           => byteCodec
        case StandardType.ShortType          => shortCodec
        case StandardType.IntType            => intCodec
        case StandardType.LongType           => longCodec
        case StandardType.FloatType          => floatCodec
        case StandardType.DoubleType         => doubleCodec
        case StandardType.BinaryType         => byteChunkCodec
        case StandardType.CharType           => charCodec
        case StandardType.UUIDType           => uuidCodec
        case StandardType.BigDecimalType     => javaBigDecimalCodec
        case StandardType.BigIntegerType     => javaBigIntegerCodec
        case StandardType.DayOfWeekType      => dayOfWeekCodec
        case StandardType.MonthType          => monthCodec
        case StandardType.MonthDayType       => monthDayCodec
        case StandardType.PeriodType         => periodCodec
        case StandardType.YearType           => yearCodec
        case StandardType.YearMonthType      => yearMonthCodec
        case StandardType.ZoneIdType         => zoneIdCodec
        case StandardType.ZoneOffsetType     => zoneOffsetCodec
        case StandardType.DurationType       => durationCodec
        case StandardType.InstantType        => instantCodec
        case StandardType.LocalDateType      => localDateCodec
        case StandardType.LocalTimeType      => localTimeCodec
        case StandardType.LocalDateTimeType  => localDateTimeCodec
        case StandardType.OffsetTimeType     => offsetTimeCodec
        case StandardType.OffsetDateTimeType => offsetDateTimeCodec
        case StandardType.ZonedDateTimeType  => zonedDateTimeCodec
      }).asInstanceOf[BinaryCodec[A]]

    override def deriveOption[A](
        option: Schema.Optional[A],
        inner: => BinaryCodec[A],
        summoned: => Option[BinaryCodec[Option[A]]]
    ): BinaryCodec[Option[A]] =
      optionCodec(inner)

    override def deriveEither[A, B](
        either: Schema.Either[A, B],
        left: => BinaryCodec[A],
        right: => BinaryCodec[B],
        summoned: => Option[BinaryCodec[Either[A, B]]]
    ): BinaryCodec[Either[A, B]] =
      eitherCodec(left, right)

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
      setCodec(inner)

    override def deriveMap[K, V](
        map: Schema.Map[K, V],
        key: => BinaryCodec[K],
        value: => BinaryCodec[V],
        summoned: => Option[BinaryCodec[Map[K, V]]]
    ): BinaryCodec[Map[K, V]] =
      mapCodec(key, value)

    override def deriveTransformedRecord[A, B](
        record: Schema.Record[A],
        transform: Schema.Transform[A, B, _],
        fields: => Chunk[Deriver.WrappedF[BinaryCodec, _]],
        summoned: => Option[BinaryCodec[B]]
    ): BinaryCodec[B] = {
      val preparedSerializationCommands =
        record.fields
          .zip(fields)
          .map { case (field, fieldCodec) =>
            (field.name, field.get, () => fieldCodec.unwrap.asInstanceOf[BinaryCodec[Any]])
          }
          .toList

      Unsafe
        .unsafe { implicit u =>
          new AdtCodec[B, RecordBuilderState](
            evolutionSteps = getEvolutionStepsFromAnnotation(record.annotations),
            typeName = record.id.name,
            constructors = Vector(record.id.name),
            transientFields = getTransientFields(record.fields),
            getSerializationCommands = (value: B) =>
              transform.g(value) match {
                case Left(failure) =>
                  List(AdtCodec.SerializationCommand.Fail(DesertFailure.SerializationFailure(failure, None)))
                case Right(value)  =>
                  preparedSerializationCommands.map { case (fieldName, getter, codec) =>
                    AdtCodec.SerializationCommand.WriteField(fieldName, getter(value), codec)
                  }
              },
            deserializationCommands = record.fields.zip(fields).toList.map { case (field, fieldInstance) =>
              fieldToDeserializationCommand(field, fieldInstance.unwrap)
            },
            initialBuilderState = () => RecordBuilderState.initial(fieldSize(record)),
            materialize = builderState =>
              record
                .construct(builderState.asChunk)
                .flatMap { a =>
                  transform.f(a)
                }
                .left
                .map(msg => DesertFailure.DeserializationFailure(msg, None))
          )
        }
    }

    private def fieldSize(value: Schema.Record[_]): Int =
      value match {
        case Schema.GenericRecord(_, _, _)                                                              => value.fields.size
        case _: Schema.CaseClass0[_]                                                                    => 0
        case _: Schema.CaseClass1[_, _]                                                                 => 1
        case _: Schema.CaseClass2[_, _, _]                                                              => 2
        case _: Schema.CaseClass3[_, _, _, _]                                                           => 3
        case _: Schema.CaseClass4[_, _, _, _, _]                                                        => 4
        case _: Schema.CaseClass5[_, _, _, _, _, _]                                                     => 5
        case _: Schema.CaseClass6[_, _, _, _, _, _, _]                                                  => 6
        case _: Schema.CaseClass7[_, _, _, _, _, _, _, _]                                               => 7
        case _: Schema.CaseClass8[_, _, _, _, _, _, _, _, _]                                            => 8
        case _: Schema.CaseClass9[_, _, _, _, _, _, _, _, _, _]                                         => 9
        case _: Schema.CaseClass10[_, _, _, _, _, _, _, _, _, _, _]                                     => 10
        case _: Schema.CaseClass11[_, _, _, _, _, _, _, _, _, _, _, _]                                  => 11
        case _: Schema.CaseClass12[_, _, _, _, _, _, _, _, _, _, _, _, _]                               => 12
        case _: Schema.CaseClass13[_, _, _, _, _, _, _, _, _, _, _, _, _, _]                            => 13
        case _: Schema.CaseClass14[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _]                         => 14
        case _: Schema.CaseClass15[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]                      => 15
        case _: Schema.CaseClass16[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]                   => 16
        case _: Schema.CaseClass17[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]                => 17
        case _: Schema.CaseClass18[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]             => 18
        case _: Schema.CaseClass19[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]          => 19
        case _: Schema.CaseClass20[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]       => 20
        case _: Schema.CaseClass21[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]    => 21
        case _: Schema.CaseClass22[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] => 22
      }

    override def deriveTupleN[T](
        schemasAndInstances: => Chunk[(Schema[_], Deriver.WrappedF[BinaryCodec, _])],
        summoned: => Option[BinaryCodec[T]]
    ): BinaryCodec[T] = {
      val (schemas, wrappedInstances) = schemasAndInstances.unzip
      val instances                   = wrappedInstances.map(_.unwrap.asInstanceOf[BinaryCodec[Any]])
      schemasAndInstances.length match {
        case 2  =>
          tuple2Codec(instances(0), fieldReaderFor(instances(0)), instances(1), fieldReaderFor(instances(1)))
            .asInstanceOf[BinaryCodec[T]]
        case 3  =>
          tuple3Codec(
            instances(0),
            fieldReaderFor(instances(0)),
            instances(1),
            fieldReaderFor(instances(1)),
            instances(2),
            fieldReaderFor(instances(2))
          )
            .asInstanceOf[BinaryCodec[T]]
        case 4  =>
          tuple4Codec(
            instances(0),
            fieldReaderFor(instances(0)),
            instances(1),
            fieldReaderFor(instances(1)),
            instances(2),
            fieldReaderFor(instances(2)),
            instances(3),
            fieldReaderFor(instances(3))
          )
            .asInstanceOf[BinaryCodec[T]]
        case 5  =>
          tuple5Codec(
            instances(0),
            fieldReaderFor(instances(0)),
            instances(1),
            fieldReaderFor(instances(1)),
            instances(2),
            fieldReaderFor(instances(2)),
            instances(3),
            fieldReaderFor(instances(3)),
            instances(4),
            fieldReaderFor(instances(4))
          )
            .asInstanceOf[BinaryCodec[T]]
        case 6  =>
          tuple6Codec(
            instances(0),
            fieldReaderFor(instances(0)),
            instances(1),
            fieldReaderFor(instances(1)),
            instances(2),
            fieldReaderFor(instances(2)),
            instances(3),
            fieldReaderFor(instances(3)),
            instances(4),
            fieldReaderFor(instances(4)),
            instances(5),
            fieldReaderFor(instances(5))
          )
            .asInstanceOf[BinaryCodec[T]]
        case 7  =>
          tuple7Codec(
            instances(0),
            fieldReaderFor(instances(0)),
            instances(1),
            fieldReaderFor(instances(1)),
            instances(2),
            fieldReaderFor(instances(2)),
            instances(3),
            fieldReaderFor(instances(3)),
            instances(4),
            fieldReaderFor(instances(4)),
            instances(5),
            fieldReaderFor(instances(5)),
            instances(6),
            fieldReaderFor(instances(6))
          )
            .asInstanceOf[BinaryCodec[T]]
        case 8  =>
          tuple8Codec(
            instances(0),
            fieldReaderFor(instances(0)),
            instances(1),
            fieldReaderFor(instances(1)),
            instances(2),
            fieldReaderFor(instances(2)),
            instances(3),
            fieldReaderFor(instances(3)),
            instances(4),
            fieldReaderFor(instances(4)),
            instances(5),
            fieldReaderFor(instances(5)),
            instances(6),
            fieldReaderFor(instances(6)),
            instances(7),
            fieldReaderFor(instances(7))
          )
            .asInstanceOf[BinaryCodec[T]]
        case 9  =>
          tuple9Codec(
            instances(0),
            fieldReaderFor(instances(0)),
            instances(1),
            fieldReaderFor(instances(1)),
            instances(2),
            fieldReaderFor(instances(2)),
            instances(3),
            fieldReaderFor(instances(3)),
            instances(4),
            fieldReaderFor(instances(4)),
            instances(5),
            fieldReaderFor(instances(5)),
            instances(6),
            fieldReaderFor(instances(6)),
            instances(7),
            fieldReaderFor(instances(7)),
            instances(8),
            fieldReaderFor(instances(8))
          )
            .asInstanceOf[BinaryCodec[T]]
        case 10 =>
          tuple10Codec(
            instances(0),
            fieldReaderFor(instances(0)),
            instances(1),
            fieldReaderFor(instances(1)),
            instances(2),
            fieldReaderFor(instances(2)),
            instances(3),
            fieldReaderFor(instances(3)),
            instances(4),
            fieldReaderFor(instances(4)),
            instances(5),
            fieldReaderFor(instances(5)),
            instances(6),
            fieldReaderFor(instances(6)),
            instances(7),
            fieldReaderFor(instances(7)),
            instances(8),
            fieldReaderFor(instances(8)),
            instances(9),
            fieldReaderFor(instances(9))
          )
            .asInstanceOf[BinaryCodec[T]]
        case 11 =>
          tuple11Codec(
            instances(0),
            fieldReaderFor(instances(0)),
            instances(1),
            fieldReaderFor(instances(1)),
            instances(2),
            fieldReaderFor(instances(2)),
            instances(3),
            fieldReaderFor(instances(3)),
            instances(4),
            fieldReaderFor(instances(4)),
            instances(5),
            fieldReaderFor(instances(5)),
            instances(6),
            fieldReaderFor(instances(6)),
            instances(7),
            fieldReaderFor(instances(7)),
            instances(8),
            fieldReaderFor(instances(8)),
            instances(9),
            fieldReaderFor(instances(9)),
            instances(10),
            fieldReaderFor(instances(10))
          )
            .asInstanceOf[BinaryCodec[T]]
        case 12 =>
          tuple12Codec(
            instances(0),
            fieldReaderFor(instances(0)),
            instances(1),
            fieldReaderFor(instances(1)),
            instances(2),
            fieldReaderFor(instances(2)),
            instances(3),
            fieldReaderFor(instances(3)),
            instances(4),
            fieldReaderFor(instances(4)),
            instances(5),
            fieldReaderFor(instances(5)),
            instances(6),
            fieldReaderFor(instances(6)),
            instances(7),
            fieldReaderFor(instances(7)),
            instances(8),
            fieldReaderFor(instances(8)),
            instances(9),
            fieldReaderFor(instances(9)),
            instances(10),
            fieldReaderFor(instances(10)),
            instances(11),
            fieldReaderFor(instances(11))
          )
            .asInstanceOf[BinaryCodec[T]]
        case 13 =>
          tuple13Codec(
            instances(0),
            fieldReaderFor(instances(0)),
            instances(1),
            fieldReaderFor(instances(1)),
            instances(2),
            fieldReaderFor(instances(2)),
            instances(3),
            fieldReaderFor(instances(3)),
            instances(4),
            fieldReaderFor(instances(4)),
            instances(5),
            fieldReaderFor(instances(5)),
            instances(6),
            fieldReaderFor(instances(6)),
            instances(7),
            fieldReaderFor(instances(7)),
            instances(8),
            fieldReaderFor(instances(8)),
            instances(9),
            fieldReaderFor(instances(9)),
            instances(10),
            fieldReaderFor(instances(10)),
            instances(11),
            fieldReaderFor(instances(11)),
            instances(12),
            fieldReaderFor(instances(12))
          )
            .asInstanceOf[BinaryCodec[T]]
        case 14 =>
          tuple14Codec(
            instances(0),
            fieldReaderFor(instances(0)),
            instances(1),
            fieldReaderFor(instances(1)),
            instances(2),
            fieldReaderFor(instances(2)),
            instances(3),
            fieldReaderFor(instances(3)),
            instances(4),
            fieldReaderFor(instances(4)),
            instances(5),
            fieldReaderFor(instances(5)),
            instances(6),
            fieldReaderFor(instances(6)),
            instances(7),
            fieldReaderFor(instances(7)),
            instances(8),
            fieldReaderFor(instances(8)),
            instances(9),
            fieldReaderFor(instances(9)),
            instances(10),
            fieldReaderFor(instances(10)),
            instances(11),
            fieldReaderFor(instances(11)),
            instances(12),
            fieldReaderFor(instances(12)),
            instances(13),
            fieldReaderFor(instances(13))
          )
            .asInstanceOf[BinaryCodec[T]]
        case 15 =>
          tuple15Codec(
            instances(0),
            fieldReaderFor(instances(0)),
            instances(1),
            fieldReaderFor(instances(1)),
            instances(2),
            fieldReaderFor(instances(2)),
            instances(3),
            fieldReaderFor(instances(3)),
            instances(4),
            fieldReaderFor(instances(4)),
            instances(5),
            fieldReaderFor(instances(5)),
            instances(6),
            fieldReaderFor(instances(6)),
            instances(7),
            fieldReaderFor(instances(7)),
            instances(8),
            fieldReaderFor(instances(8)),
            instances(9),
            fieldReaderFor(instances(9)),
            instances(10),
            fieldReaderFor(instances(10)),
            instances(11),
            fieldReaderFor(instances(11)),
            instances(12),
            fieldReaderFor(instances(12)),
            instances(13),
            fieldReaderFor(instances(13)),
            instances(14),
            fieldReaderFor(instances(14))
          )
            .asInstanceOf[BinaryCodec[T]]
        case 16 =>
          tuple16Codec(
            instances(0),
            fieldReaderFor(instances(0)),
            instances(1),
            fieldReaderFor(instances(1)),
            instances(2),
            fieldReaderFor(instances(2)),
            instances(3),
            fieldReaderFor(instances(3)),
            instances(4),
            fieldReaderFor(instances(4)),
            instances(5),
            fieldReaderFor(instances(5)),
            instances(6),
            fieldReaderFor(instances(6)),
            instances(7),
            fieldReaderFor(instances(7)),
            instances(8),
            fieldReaderFor(instances(8)),
            instances(9),
            fieldReaderFor(instances(9)),
            instances(10),
            fieldReaderFor(instances(10)),
            instances(11),
            fieldReaderFor(instances(11)),
            instances(12),
            fieldReaderFor(instances(12)),
            instances(13),
            fieldReaderFor(instances(13)),
            instances(14),
            fieldReaderFor(instances(14)),
            instances(15),
            fieldReaderFor(instances(15))
          )
            .asInstanceOf[BinaryCodec[T]]
        case 17 =>
          tuple17Codec(
            instances(0),
            fieldReaderFor(instances(0)),
            instances(1),
            fieldReaderFor(instances(1)),
            instances(2),
            fieldReaderFor(instances(2)),
            instances(3),
            fieldReaderFor(instances(3)),
            instances(4),
            fieldReaderFor(instances(4)),
            instances(5),
            fieldReaderFor(instances(5)),
            instances(6),
            fieldReaderFor(instances(6)),
            instances(7),
            fieldReaderFor(instances(7)),
            instances(8),
            fieldReaderFor(instances(8)),
            instances(9),
            fieldReaderFor(instances(9)),
            instances(10),
            fieldReaderFor(instances(10)),
            instances(11),
            fieldReaderFor(instances(11)),
            instances(12),
            fieldReaderFor(instances(12)),
            instances(13),
            fieldReaderFor(instances(13)),
            instances(14),
            fieldReaderFor(instances(14)),
            instances(15),
            fieldReaderFor(instances(15)),
            instances(16),
            fieldReaderFor(instances(16))
          )
            .asInstanceOf[BinaryCodec[T]]
        case 18 =>
          tuple18Codec(
            instances(0),
            fieldReaderFor(instances(0)),
            instances(1),
            fieldReaderFor(instances(1)),
            instances(2),
            fieldReaderFor(instances(2)),
            instances(3),
            fieldReaderFor(instances(3)),
            instances(4),
            fieldReaderFor(instances(4)),
            instances(5),
            fieldReaderFor(instances(5)),
            instances(6),
            fieldReaderFor(instances(6)),
            instances(7),
            fieldReaderFor(instances(7)),
            instances(8),
            fieldReaderFor(instances(8)),
            instances(9),
            fieldReaderFor(instances(9)),
            instances(10),
            fieldReaderFor(instances(10)),
            instances(11),
            fieldReaderFor(instances(11)),
            instances(12),
            fieldReaderFor(instances(12)),
            instances(13),
            fieldReaderFor(instances(13)),
            instances(14),
            fieldReaderFor(instances(14)),
            instances(15),
            fieldReaderFor(instances(15)),
            instances(16),
            fieldReaderFor(instances(16)),
            instances(17),
            fieldReaderFor(instances(17))
          )
            .asInstanceOf[BinaryCodec[T]]
        case 19 =>
          tuple19Codec(
            instances(0),
            fieldReaderFor(instances(0)),
            instances(1),
            fieldReaderFor(instances(1)),
            instances(2),
            fieldReaderFor(instances(2)),
            instances(3),
            fieldReaderFor(instances(3)),
            instances(4),
            fieldReaderFor(instances(4)),
            instances(5),
            fieldReaderFor(instances(5)),
            instances(6),
            fieldReaderFor(instances(6)),
            instances(7),
            fieldReaderFor(instances(7)),
            instances(8),
            fieldReaderFor(instances(8)),
            instances(9),
            fieldReaderFor(instances(9)),
            instances(10),
            fieldReaderFor(instances(10)),
            instances(11),
            fieldReaderFor(instances(11)),
            instances(12),
            fieldReaderFor(instances(12)),
            instances(13),
            fieldReaderFor(instances(13)),
            instances(14),
            fieldReaderFor(instances(14)),
            instances(15),
            fieldReaderFor(instances(15)),
            instances(16),
            fieldReaderFor(instances(16)),
            instances(17),
            fieldReaderFor(instances(17)),
            instances(18),
            fieldReaderFor(instances(18))
          )
            .asInstanceOf[BinaryCodec[T]]
        case 20 =>
          tuple20Codec(
            instances(0),
            fieldReaderFor(instances(0)),
            instances(1),
            fieldReaderFor(instances(1)),
            instances(2),
            fieldReaderFor(instances(2)),
            instances(3),
            fieldReaderFor(instances(3)),
            instances(4),
            fieldReaderFor(instances(4)),
            instances(5),
            fieldReaderFor(instances(5)),
            instances(6),
            fieldReaderFor(instances(6)),
            instances(7),
            fieldReaderFor(instances(7)),
            instances(8),
            fieldReaderFor(instances(8)),
            instances(9),
            fieldReaderFor(instances(9)),
            instances(10),
            fieldReaderFor(instances(10)),
            instances(11),
            fieldReaderFor(instances(11)),
            instances(12),
            fieldReaderFor(instances(12)),
            instances(13),
            fieldReaderFor(instances(13)),
            instances(14),
            fieldReaderFor(instances(14)),
            instances(15),
            fieldReaderFor(instances(15)),
            instances(16),
            fieldReaderFor(instances(16)),
            instances(17),
            fieldReaderFor(instances(17)),
            instances(18),
            fieldReaderFor(instances(18)),
            instances(19),
            fieldReaderFor(instances(19))
          )
            .asInstanceOf[BinaryCodec[T]]
        case 21 =>
          tuple21Codec(
            instances(0),
            fieldReaderFor(instances(0)),
            instances(1),
            fieldReaderFor(instances(1)),
            instances(2),
            fieldReaderFor(instances(2)),
            instances(3),
            fieldReaderFor(instances(3)),
            instances(4),
            fieldReaderFor(instances(4)),
            instances(5),
            fieldReaderFor(instances(5)),
            instances(6),
            fieldReaderFor(instances(6)),
            instances(7),
            fieldReaderFor(instances(7)),
            instances(8),
            fieldReaderFor(instances(8)),
            instances(9),
            fieldReaderFor(instances(9)),
            instances(10),
            fieldReaderFor(instances(10)),
            instances(11),
            fieldReaderFor(instances(11)),
            instances(12),
            fieldReaderFor(instances(12)),
            instances(13),
            fieldReaderFor(instances(13)),
            instances(14),
            fieldReaderFor(instances(14)),
            instances(15),
            fieldReaderFor(instances(15)),
            instances(16),
            fieldReaderFor(instances(16)),
            instances(17),
            fieldReaderFor(instances(17)),
            instances(18),
            fieldReaderFor(instances(18)),
            instances(19),
            fieldReaderFor(instances(19)),
            instances(20),
            fieldReaderFor(instances(20))
          )
            .asInstanceOf[BinaryCodec[T]]
        case 22 =>
          tuple22Codec(
            instances(0),
            fieldReaderFor(instances(0)),
            instances(1),
            fieldReaderFor(instances(1)),
            instances(2),
            fieldReaderFor(instances(2)),
            instances(3),
            fieldReaderFor(instances(3)),
            instances(4),
            fieldReaderFor(instances(4)),
            instances(5),
            fieldReaderFor(instances(5)),
            instances(6),
            fieldReaderFor(instances(6)),
            instances(7),
            fieldReaderFor(instances(7)),
            instances(8),
            fieldReaderFor(instances(8)),
            instances(9),
            fieldReaderFor(instances(9)),
            instances(10),
            fieldReaderFor(instances(10)),
            instances(11),
            fieldReaderFor(instances(11)),
            instances(12),
            fieldReaderFor(instances(12)),
            instances(13),
            fieldReaderFor(instances(13)),
            instances(14),
            fieldReaderFor(instances(14)),
            instances(15),
            fieldReaderFor(instances(15)),
            instances(16),
            fieldReaderFor(instances(16)),
            instances(17),
            fieldReaderFor(instances(17)),
            instances(18),
            fieldReaderFor(instances(18)),
            instances(19),
            fieldReaderFor(instances(19)),
            instances(20),
            fieldReaderFor(instances(20)),
            instances(21),
            fieldReaderFor(instances(21))
          )
            .asInstanceOf[BinaryCodec[T]]
      }
    }

    private def fieldReaderFor(instance: BinaryCodec[_]): TupleFieldReader[Any] =
      instance match {
        case optional: OptionBinaryCodec[_] =>
          TupleFieldReader
            .optionalFieldReader[Any](optional.asInstanceOf[BinaryCodec[Any]])
            .asInstanceOf[TupleFieldReader[Any]]
        case _                              =>
          TupleFieldReader.requiredFieldReader[Any](instance.asInstanceOf[BinaryCodec[Any]])
      }

    private def getEvolutionStepsFromAnnotation(value: Chunk[Any]): Vector[Evolution] =
      value.collectFirst { case ann: evolutionSteps => ann } match {
        case None             => Vector(Evolution.InitialVersion)
        case Some(annotation) => Evolution.InitialVersion +: annotation.steps.toVector
      }

    private def isTransient(c: Schema.Case[_, _]): Boolean =
      c.annotations.contains(transientConstructor())

    private def getTransientFields(fields: Chunk[Schema.Field[_, _]]): Map[String, Any] =
      fields.flatMap { field =>
        field.annotations.collect { case transientField(defaultValue) =>
          field.name -> defaultValue
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
