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
import io.github.vigoo.desert.ziosupport.codecs._
import zio.Chunk
import zio.schema.{Schema, StandardType}

object DerivedBinaryCodec {

  def derive[T](implicit schema: Schema[T]): BinaryCodec[T] =
    schema match {
      case enum: Schema.Enum[_]                        =>
        deriveEnum(getEvolutionStepsFromAnnotation(enum.annotations), enum)
      case genericRecord: Schema.GenericRecord         =>
        deriveRecord(getEvolutionStepsFromAnnotation(genericRecord.annotations), genericRecord)
          .asInstanceOf[BinaryCodec[T]]
      case caseClass1: Schema.CaseClass1[_, _]         =>
        deriveRecord(getEvolutionStepsFromAnnotation(caseClass1.annotations), caseClass1)
          .asInstanceOf[BinaryCodec[T]]
      case caseClass2: Schema.CaseClass2[_, _, _]      =>
        deriveRecord(getEvolutionStepsFromAnnotation(caseClass2.annotations), caseClass2)
          .asInstanceOf[BinaryCodec[T]]
      case transform: Schema.Transform[_, _, _]        =>
        val binaryCodec = derive(transform.codec).asInstanceOf[BinaryCodec[Any]]
        BinaryCodec.from(
          binaryCodec.contramapOrFail(
            transform.f.asInstanceOf[Any => Either[String, Any]](_).left.map(SerializationFailure(_, None))
          ),
          binaryCodec.mapOrFail(
            transform.g.asInstanceOf[Any => Either[String, T]](_).left.map(DeserializationFailure(_, None))
          )
        )
      case Schema.Lazy(inner)                          =>
        derive(inner())
      case map: Schema.MapSchema[_, _]                 =>
        codecs.mapCodec(derive(map.ks), derive(map.vs)).asInstanceOf[BinaryCodec[T]]
      case set: Schema.SetSchema[_]                    =>
        codecs.setCodec(derive(set.as)).asInstanceOf[BinaryCodec[T]]
      case sequence: Schema.Sequence[_, _, _]          =>
        val baseCodec = chunkCodec[Any](
          derive(sequence.schemaA.asInstanceOf[Schema[Any]])
        )
        BinaryCodec.from(
          baseCodec.contramap(sequence.toChunk.asInstanceOf[T => Chunk[Any]]),
          baseCodec.map(sequence.fromChunk.asInstanceOf[Chunk[Any] => T])
        )
      case Schema.Primitive(standardType, annotations) =>
        (standardType match {
          case StandardType.UnitType                      => codecs.unitCodec
          case StandardType.StringType                    => codecs.stringCodec
          case StandardType.BoolType                      => codecs.booleanCodec
          case StandardType.ByteType                      => codecs.byteCodec
          case StandardType.ShortType                     => codecs.shortCodec
          case StandardType.IntType                       => codecs.intCodec
          case StandardType.LongType                      => codecs.longCodec
          case StandardType.FloatType                     => codecs.floatCodec
          case StandardType.DoubleType                    => codecs.doubleCodec
          case StandardType.BinaryType                    => byteChunkCodec
          case StandardType.CharType                      => codecs.charCodec
          case StandardType.UUIDType                      => codecs.uuidCodec
          case StandardType.BigDecimalType                => ???
          case StandardType.BigIntegerType                => ???
          case StandardType.DayOfWeekType                 => ???
          case StandardType.MonthType                     => ???
          case StandardType.MonthDayType                  => ???
          case StandardType.PeriodType                    => ???
          case StandardType.YearType                      => ???
          case StandardType.YearMonthType                 => ???
          case StandardType.ZoneIdType                    => ???
          case StandardType.ZoneOffsetType                => ???
          case StandardType.DurationType                  => ???
          case StandardType.InstantType(formatter)        => ???
          case StandardType.LocalDateType(formatter)      => ???
          case StandardType.LocalTimeType(formatter)      => ???
          case StandardType.LocalDateTimeType(formatter)  => ???
          case StandardType.OffsetTimeType(formatter)     => ???
          case StandardType.OffsetDateTimeType(formatter) => ???
          case StandardType.ZonedDateTimeType(formatter)  => ???
        }).asInstanceOf[BinaryCodec[T]]

      case Schema.Optional(codec, annotations)           =>
        codecs.optionCodec(derive(codec)).asInstanceOf[BinaryCodec[T]]
      case Schema.Fail(message, annotations)             =>
        BinaryCodec.from(
          (_: T) => failSerializerWith(SerializationFailure(message, None)),
          () => failDeserializerWith[T](DeserializationFailure(message, None))
        )
      case Schema.Tuple(left, right, annotations)        =>
        // TODO: flatten tuple
        ???
      case Schema.EitherSchema(left, right, annotations) =>
        codecs
          .eitherCodec(
            derive(left),
            derive(right)
          )
          .asInstanceOf[BinaryCodec[T]]
      case Schema.Meta(ast, annotations)                 =>
        schemaAstCodec.asInstanceOf[BinaryCodec[T]]
      case Schema.Dynamic(annotations)                   =>
        dynamicValueCodec.asInstanceOf[BinaryCodec[T]]
      case Schema.SemiDynamic(defaultValue, annotations) =>
        ???
    }

  private def deriveRecord[T, S <: Schema.Record[_]](evolutionSteps: Vector[Evolution], schema: S)(implicit
      recordSerializer: RecordSerializer[S]
  ): BinaryCodec[T] =
    new AdtCodec[T, SchemaBuilderState](
      evolutionSteps = evolutionSteps,
      typeName = schema.id.name,
      constructors = Vector(schema.id.name),
      transientFields = getTransientFields(schema.structure),
      getSerializationCommands = (value: T) => recordSerializer.getSerializationCommands(schema, value),
      deserializationCommands = ???,
      initialBuilderState = ???,
      materialize = ???
    )

  private def deriveEnum[T](evolutionSteps: Vector[Evolution], schema: Schema.Enum[T]): BinaryCodec[T] =
    new AdtCodec[T, SchemaBuilderState](
      evolutionSteps = ???,
      typeName = schema.id.name,
      constructors = ???,
      transientFields = ???,
      getSerializationCommands = ???,
      deserializationCommands = ???,
      initialBuilderState = ???,
      materialize = ???
    )

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

  private case class SchemaBuilderState()
}
