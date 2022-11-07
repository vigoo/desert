package io.github.vigoo.desert.zioschema

import io.github.vigoo.desert.{
  AdtCodec,
  BinaryCodec,
  DeserializationFailure,
  Evolution,
  InitialVersion,
  SerializationFailure,
  codecs,
  evolutionSteps,
  transientField
}
import io.github.vigoo.desert.syntax._
import io.github.vigoo.desert.zioschema.RecordDeserializer.SchemaBuilderState
import io.github.vigoo.desert.ziosupport.codecs._
import zio.{Chunk, ChunkBuilder}
import zio.schema.{Schema, StandardType}

import scala.annotation.tailrec

object DerivedBinaryCodec {

  def derive[T](implicit schema: Schema[T]): BinaryCodec[T] =
    schema match {
      case enum: Schema.Enum[_]                      =>
        deriveEnum(getEvolutionStepsFromAnnotation(enum.annotations), enum)
      case genericRecord: Schema.GenericRecord       =>
        deriveRecord(getEvolutionStepsFromAnnotation(genericRecord.annotations), genericRecord)
          .asInstanceOf[BinaryCodec[T]]
      case caseClass1: Schema.CaseClass1[_, _]       =>
        deriveRecord(getEvolutionStepsFromAnnotation(caseClass1.annotations), caseClass1)
          .asInstanceOf[BinaryCodec[T]]
      case caseClass2: Schema.CaseClass2[_, _, _]    =>
        deriveRecord(getEvolutionStepsFromAnnotation(caseClass2.annotations), caseClass2)
          .asInstanceOf[BinaryCodec[T]]
      case caseClass3: Schema.CaseClass3[_, _, _, _] =>
        deriveRecord(getEvolutionStepsFromAnnotation(caseClass3.annotations), caseClass3)
          .asInstanceOf[BinaryCodec[T]]
      case transform: Schema.Transform[_, _, _]      =>
        val binaryCodec = derive(transform.codec).asInstanceOf[BinaryCodec[Any]]
        BinaryCodec.from(
          binaryCodec.contramapOrFail(
            transform.f.asInstanceOf[Any => Either[String, Any]](_).left.map(SerializationFailure(_, None))
          ),
          binaryCodec.mapOrFail(
            transform.g.asInstanceOf[Any => Either[String, T]](_).left.map(DeserializationFailure(_, None))
          )
        )
      case Schema.Lazy(inner)                        =>
        derive(inner())
      case map: Schema.MapSchema[_, _]               =>
        codecs.mapCodec(derive(map.ks), derive(map.vs)).asInstanceOf[BinaryCodec[T]]
      case set: Schema.SetSchema[_]                  =>
        codecs.setCodec(derive(set.as)).asInstanceOf[BinaryCodec[T]]
      case sequence: Schema.Sequence[_, _, _]        =>
        val baseCodec = chunkCodec[Any](
          derive(sequence.schemaA.asInstanceOf[Schema[Any]])
        )
        BinaryCodec.from(
          baseCodec.contramap(sequence.toChunk.asInstanceOf[T => Chunk[Any]]),
          baseCodec.map(sequence.fromChunk.asInstanceOf[Chunk[Any] => T])
        )
      case Schema.Primitive(standardType, _)         =>
        (standardType match {
          case StandardType.UnitType              => codecs.unitCodec
          case StandardType.StringType            => codecs.stringCodec
          case StandardType.BoolType              => codecs.booleanCodec
          case StandardType.ByteType              => codecs.byteCodec
          case StandardType.ShortType             => codecs.shortCodec
          case StandardType.IntType               => codecs.intCodec
          case StandardType.LongType              => codecs.longCodec
          case StandardType.FloatType             => codecs.floatCodec
          case StandardType.DoubleType            => codecs.doubleCodec
          case StandardType.BinaryType            => byteChunkCodec
          case StandardType.CharType              => codecs.charCodec
          case StandardType.UUIDType              => codecs.uuidCodec
          case StandardType.BigDecimalType        => codecs.javaBigDecimalCodec
          case StandardType.BigIntegerType        => codecs.javaBigIntegerCodec
          case StandardType.DayOfWeekType         => codecs.dayOfWeekCodec
          case StandardType.MonthType             => codecs.monthCodec
          case StandardType.MonthDayType          => codecs.monthDayCodec
          case StandardType.PeriodType            => codecs.periodCodec
          case StandardType.YearType              => codecs.yearCodec
          case StandardType.YearMonthType         => codecs.yearMonthCodec
          case StandardType.ZoneIdType            => codecs.zoneIdCodec
          case StandardType.ZoneOffsetType        => codecs.zoneOffsetCodec
          case StandardType.DurationType          => codecs.durationCodec
          case StandardType.InstantType(_)        => codecs.instantCodec
          case StandardType.LocalDateType(_)      => codecs.localDateCodec
          case StandardType.LocalTimeType(_)      => codecs.localTimeCodec
          case StandardType.LocalDateTimeType(_)  => codecs.localDateTimeCodec
          case StandardType.OffsetTimeType(_)     => codecs.offsetTimeCodec
          case StandardType.OffsetDateTimeType(_) => codecs.offsetDateTimeCodec
          case StandardType.ZonedDateTimeType(_)  => codecs.zonedDateTimeCodec
        }).asInstanceOf[BinaryCodec[T]]

      case Schema.Optional(codec, _)           =>
        codecs.optionCodec(derive(codec)).asInstanceOf[BinaryCodec[T]]
      case Schema.Fail(message, _)             =>
        BinaryCodec.from(
          (_: T) => failSerializerWith(SerializationFailure(message, None)),
          () => failDeserializerWith[T](DeserializationFailure(message, None))
        )
      case Schema.Tuple(left, right, _)        =>
        deriveTuple(left, right)
      case Schema.EitherSchema(left, right, _) =>
        codecs
          .eitherCodec(
            derive(left),
            derive(right)
          )
          .asInstanceOf[BinaryCodec[T]]
      case Schema.Meta(_, _)                   =>
        schemaAstCodec.asInstanceOf[BinaryCodec[T]]
      case Schema.Dynamic(_)                   =>
        dynamicValueCodec.asInstanceOf[BinaryCodec[T]]
      case Schema.SemiDynamic(_, _)            =>
        // NOTE: SemiDynamic is going to be removed from zio-schema, not supporting here
        ???
    }

  private def deriveRecord[T, S <: Schema.Record[_]](evolutionSteps: Vector[Evolution], schema: S)(implicit
      recordSerializer: RecordSerializer[S],
      recordDeserializer: RecordDeserializer[S]
  ): BinaryCodec[T] =
    new AdtCodec[T, SchemaBuilderState](
      evolutionSteps = evolutionSteps,
      typeName = schema.id.name,
      constructors = Vector(schema.id.name),
      transientFields = getTransientFields(schema.structure),
      getSerializationCommands = (value: T) => recordSerializer.getSerializationCommands(schema, value),
      deserializationCommands = recordDeserializer.getDeserializationCommands(schema),
      initialBuilderState = SchemaBuilderState.initial,
      materialize = builderState =>
        schema
          .rawConstruct(builderState.asChunk)
          .map(_.asInstanceOf[T])
          .left
          .map(msg => DeserializationFailure(msg, None))
    )

  private def deriveEnum[T](evolutionSteps: Vector[Evolution], schema: Schema.Enum[T]): BinaryCodec[T] =
    codecs.unitCodec.asInstanceOf[BinaryCodec[T]]
//    new AdtCodec[T, SchemaBuilderState](
//      evolutionSteps = evolutionSteps,
//      typeName = schema.id.name,
//      constructors = ???,
//      transientFields = ???,
//      getSerializationCommands = ???,
//      deserializationCommands = ???,
//      initialBuilderState = ???,
//      materialize = ???
//    )

  private def deriveTuple[T](left: Schema[Any], right: Schema[Any]): BinaryCodec[T] = {
    val fields = ChunkBuilder.make[Schema[Any]](2)
    gatherTupleFields(left, fields)
    fields += right
    deriveNTuple(fields.result().reverse)
  }

  @tailrec
  private def gatherTupleFields(schema: Schema[Any], fields: ChunkBuilder[Schema[Any]]): Unit =
    schema.asInstanceOf[Schema[_]] match {
      case Schema.Tuple(left, right, _) =>
        fields += right
        gatherTupleFields(left, fields)
      case _                            =>
        fields += schema
    }

  private def deriveNTuple[T](values: Chunk[Schema[Any]]): BinaryCodec[T] =
    values.length match {
      case 2 =>
        codecs
          .tuple2Codec(
            derive(values(0)),
            fieldReaderFor(values(0)),
            derive(values(1)),
            fieldReaderFor(values(1))
          )
          .asInstanceOf[BinaryCodec[T]]
    }

  private def fieldReaderFor(schema: Schema[_]): codecs.TupleFieldReader[Any] =
    RecordDeserializer.findTopLevelOptionalNode(schema) match {
      case Some(optional) =>
        codecs.TupleFieldReader
          .optionalFieldReader[Any](derive(optional.codec.asInstanceOf[Schema[Any]]))
          .asInstanceOf[codecs.TupleFieldReader[Any]]
      case None           =>
        codecs.TupleFieldReader.requiredFieldReader[Any](derive(schema.asInstanceOf[Schema[Any]]))
    }

  private def getTransientFields(fields: Chunk[Schema.Field[_]]): Map[Symbol, Any] =
    fields.flatMap { field =>
      field.annotations.collect { case transientField(defaultValue) =>
        Symbol(field.label) -> defaultValue
      }
    }.toMap

  private def getEvolutionStepsFromAnnotation(value: Chunk[Any]): Vector[Evolution] =
    value.collectFirst { case ann: evolutionSteps => ann } match {
      case None             => Vector(InitialVersion)
      case Some(annotation) => InitialVersion +: annotation.steps.toVector
    }
}
