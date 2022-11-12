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
import zio.{Chunk, ChunkBuilder, Unsafe}
import zio.schema.{CaseSet, Schema, StandardType, TypeId}

import scala.annotation.tailrec
import scala.reflect.ClassTag

object DerivedBinaryCodec {
  private lazy val globalDerivationContext = new DerivationContext

  def derive[T](implicit schema: Schema[T]): BinaryCodec[T] = {
//    implicit val context: DerivationContext = new DerivationContext
    implicit val context: DerivationContext = globalDerivationContext
    deriveInContext[T](schema)
  }

  private[zioschema] def deriveInContext[T](
      schema: Schema[T]
  )(implicit derivationContext: DerivationContext): BinaryCodec[T] =
    derivationContext.get(schema) match {
      case None        =>
        val derived = deriveImpl[T](schema, derivationContext)
        val codec   = derivationContext.put(schema, derived)
        codec
      case Some(codec) =>
        codec
    }

  private[zioschema] def deriveImpl[T](implicit
      schema: Schema[T],
      derivationContext: DerivationContext
  ): BinaryCodec[T] =
    if (schema == schemas.builtInThrowableCodecPlaceholder) codecs.throwableCodec.asInstanceOf[BinaryCodec[T]]
    else
      schema match {
        case e: Schema.Enum1[_, _]                                                                 =>
          deriveEnum(getEvolutionStepsFromAnnotation(e.annotations), e).asInstanceOf[BinaryCodec[T]]
        case e: Schema.Enum2[_, _, _]                                                              =>
          deriveEnum(getEvolutionStepsFromAnnotation(e.annotations), e).asInstanceOf[BinaryCodec[T]]
        case e: Schema.Enum3[_, _, _, _]                                                           =>
          deriveEnum(getEvolutionStepsFromAnnotation(e.annotations), e).asInstanceOf[BinaryCodec[T]]
        case e: Schema.Enum4[_, _, _, _, _]                                                        =>
          deriveEnum(getEvolutionStepsFromAnnotation(e.annotations), e).asInstanceOf[BinaryCodec[T]]
        case e: Schema.Enum5[_, _, _, _, _, _]                                                     =>
          deriveEnum(getEvolutionStepsFromAnnotation(e.annotations), e).asInstanceOf[BinaryCodec[T]]
        case e: Schema.Enum6[_, _, _, _, _, _, _]                                                  =>
          deriveEnum(getEvolutionStepsFromAnnotation(e.annotations), e).asInstanceOf[BinaryCodec[T]]
        case e: Schema.Enum7[_, _, _, _, _, _, _, _]                                               =>
          deriveEnum(getEvolutionStepsFromAnnotation(e.annotations), e).asInstanceOf[BinaryCodec[T]]
        case e: Schema.Enum8[_, _, _, _, _, _, _, _, _]                                            =>
          deriveEnum(getEvolutionStepsFromAnnotation(e.annotations), e).asInstanceOf[BinaryCodec[T]]
        case e: Schema.Enum9[_, _, _, _, _, _, _, _, _, _]                                         =>
          deriveEnum(getEvolutionStepsFromAnnotation(e.annotations), e).asInstanceOf[BinaryCodec[T]]
        case e: Schema.Enum10[_, _, _, _, _, _, _, _, _, _, _]                                     =>
          deriveEnum(getEvolutionStepsFromAnnotation(e.annotations), e).asInstanceOf[BinaryCodec[T]]
        case e: Schema.Enum11[_, _, _, _, _, _, _, _, _, _, _, _]                                  =>
          deriveEnum(getEvolutionStepsFromAnnotation(e.annotations), e).asInstanceOf[BinaryCodec[T]]
        case e: Schema.Enum12[_, _, _, _, _, _, _, _, _, _, _, _, _]                               =>
          deriveEnum(getEvolutionStepsFromAnnotation(e.annotations), e).asInstanceOf[BinaryCodec[T]]
        case e: Schema.Enum13[_, _, _, _, _, _, _, _, _, _, _, _, _, _]                            =>
          deriveEnum(getEvolutionStepsFromAnnotation(e.annotations), e).asInstanceOf[BinaryCodec[T]]
        case e: Schema.Enum14[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _]                         =>
          deriveEnum(getEvolutionStepsFromAnnotation(e.annotations), e).asInstanceOf[BinaryCodec[T]]
        case e: Schema.Enum15[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]                      =>
          deriveEnum(getEvolutionStepsFromAnnotation(e.annotations), e).asInstanceOf[BinaryCodec[T]]
        case e: Schema.Enum16[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]                   =>
          deriveEnum(getEvolutionStepsFromAnnotation(e.annotations), e).asInstanceOf[BinaryCodec[T]]
        case e: Schema.Enum17[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]                =>
          deriveEnum(getEvolutionStepsFromAnnotation(e.annotations), e).asInstanceOf[BinaryCodec[T]]
        case e: Schema.Enum18[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]             =>
          deriveEnum(getEvolutionStepsFromAnnotation(e.annotations), e).asInstanceOf[BinaryCodec[T]]
        case e: Schema.Enum19[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]          =>
          deriveEnum(getEvolutionStepsFromAnnotation(e.annotations), e).asInstanceOf[BinaryCodec[T]]
        case e: Schema.Enum20[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]       =>
          deriveEnum(getEvolutionStepsFromAnnotation(e.annotations), e).asInstanceOf[BinaryCodec[T]]
        case e: Schema.Enum21[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]    =>
          deriveEnum(getEvolutionStepsFromAnnotation(e.annotations), e).asInstanceOf[BinaryCodec[T]]
        case e: Schema.Enum22[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] =>
          deriveEnum(getEvolutionStepsFromAnnotation(e.annotations), e).asInstanceOf[BinaryCodec[T]]

        case e: Schema.EnumN[_, _]               =>
          deriveEnum(
            getEvolutionStepsFromAnnotation(e.annotations),
            e.asInstanceOf[Schema.EnumN[Any, CaseSet.Aux[Any]]]
          )(
            EnumSerializer.enumNSerializer,
            EnumDeserializer.enumNDeserializer
          ).asInstanceOf[BinaryCodec[T]]
        case genericRecord: Schema.GenericRecord =>
          deriveRecord(getEvolutionStepsFromAnnotation(genericRecord.annotations), genericRecord)
            .asInstanceOf[BinaryCodec[T]]

        case cc: Schema.CaseClass0[_]                                                                    =>
          deriveRecord(getEvolutionStepsFromAnnotation(cc.annotations), cc)
            .asInstanceOf[BinaryCodec[T]]
        case cc: Schema.CaseClass1[_, _]                                                                 =>
          if (cc.id == schemas.builtInArrayCodecTypeId) {
            val classTag = cc.annotations.head.asInstanceOf[ClassTag[Any]]
            codecs
              .arrayCodec(deriveInContext(cc.field.schema).asInstanceOf[BinaryCodec[Any]], classTag)
              .asInstanceOf[BinaryCodec[T]]
          } else if (cc.id == schemas.builtInTryCodecTypeId) {
            codecs
              .tryCodec(deriveInContext(cc.field.schema).asInstanceOf[BinaryCodec[Any]])
              .asInstanceOf[BinaryCodec[T]]
          } else {
            deriveRecord(getEvolutionStepsFromAnnotation(cc.annotations), cc)
              .asInstanceOf[BinaryCodec[T]]
          }
        case cc: Schema.CaseClass2[_, _, _]                                                              =>
          deriveRecord(getEvolutionStepsFromAnnotation(cc.annotations), cc)
            .asInstanceOf[BinaryCodec[T]]
        case cc: Schema.CaseClass3[_, _, _, _]                                                           =>
          deriveRecord(getEvolutionStepsFromAnnotation(cc.annotations), cc)
            .asInstanceOf[BinaryCodec[T]]
        case cc: Schema.CaseClass4[_, _, _, _, _]                                                        =>
          deriveRecord(getEvolutionStepsFromAnnotation(cc.annotations), cc)
            .asInstanceOf[BinaryCodec[T]]
        case cc: Schema.CaseClass5[_, _, _, _, _, _]                                                     =>
          deriveRecord(getEvolutionStepsFromAnnotation(cc.annotations), cc)
            .asInstanceOf[BinaryCodec[T]]
        case cc: Schema.CaseClass6[_, _, _, _, _, _, _]                                                  =>
          deriveRecord(getEvolutionStepsFromAnnotation(cc.annotations), cc)
            .asInstanceOf[BinaryCodec[T]]
        case cc: Schema.CaseClass7[_, _, _, _, _, _, _, _]                                               =>
          deriveRecord(getEvolutionStepsFromAnnotation(cc.annotations), cc)
            .asInstanceOf[BinaryCodec[T]]
        case cc: Schema.CaseClass8[_, _, _, _, _, _, _, _, _]                                            =>
          deriveRecord(getEvolutionStepsFromAnnotation(cc.annotations), cc)
            .asInstanceOf[BinaryCodec[T]]
        case cc: Schema.CaseClass9[_, _, _, _, _, _, _, _, _, _]                                         =>
          deriveRecord(getEvolutionStepsFromAnnotation(cc.annotations), cc)
            .asInstanceOf[BinaryCodec[T]]
        case cc: Schema.CaseClass10[_, _, _, _, _, _, _, _, _, _, _]                                     =>
          deriveRecord(getEvolutionStepsFromAnnotation(cc.annotations), cc)
            .asInstanceOf[BinaryCodec[T]]
        case cc: Schema.CaseClass11[_, _, _, _, _, _, _, _, _, _, _, _]                                  =>
          deriveRecord(getEvolutionStepsFromAnnotation(cc.annotations), cc)
            .asInstanceOf[BinaryCodec[T]]
        case cc: Schema.CaseClass12[_, _, _, _, _, _, _, _, _, _, _, _, _]                               =>
          deriveRecord(getEvolutionStepsFromAnnotation(cc.annotations), cc)
            .asInstanceOf[BinaryCodec[T]]
        case cc: Schema.CaseClass13[_, _, _, _, _, _, _, _, _, _, _, _, _, _]                            =>
          deriveRecord(getEvolutionStepsFromAnnotation(cc.annotations), cc)
            .asInstanceOf[BinaryCodec[T]]
        case cc: Schema.CaseClass14[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _]                         =>
          deriveRecord(getEvolutionStepsFromAnnotation(cc.annotations), cc)
            .asInstanceOf[BinaryCodec[T]]
        case cc: Schema.CaseClass15[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]                      =>
          deriveRecord(getEvolutionStepsFromAnnotation(cc.annotations), cc)
            .asInstanceOf[BinaryCodec[T]]
        case cc: Schema.CaseClass16[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]                   =>
          deriveRecord(getEvolutionStepsFromAnnotation(cc.annotations), cc)
            .asInstanceOf[BinaryCodec[T]]
        case cc: Schema.CaseClass17[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]                =>
          deriveRecord(getEvolutionStepsFromAnnotation(cc.annotations), cc)
            .asInstanceOf[BinaryCodec[T]]
        case cc: Schema.CaseClass18[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]             =>
          deriveRecord(getEvolutionStepsFromAnnotation(cc.annotations), cc)
            .asInstanceOf[BinaryCodec[T]]
        case cc: Schema.CaseClass19[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]          =>
          deriveRecord(getEvolutionStepsFromAnnotation(cc.annotations), cc)
            .asInstanceOf[BinaryCodec[T]]
        case cc: Schema.CaseClass20[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]       =>
          deriveRecord(getEvolutionStepsFromAnnotation(cc.annotations), cc)
            .asInstanceOf[BinaryCodec[T]]
        case cc: Schema.CaseClass21[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]    =>
          deriveRecord(getEvolutionStepsFromAnnotation(cc.annotations), cc)
            .asInstanceOf[BinaryCodec[T]]
        case cc: Schema.CaseClass22[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] =>
          deriveRecord(getEvolutionStepsFromAnnotation(cc.annotations), cc)
            .asInstanceOf[BinaryCodec[T]]
        case transform: Schema.Transform[_, _, _]                                                        =>
          val binaryCodec = deriveInContext(transform.schema).asInstanceOf[BinaryCodec[Any]]
          BinaryCodec.from(
            binaryCodec.contramapOrFail(
              transform.g.asInstanceOf[Any => Either[String, Any]](_).left.map(SerializationFailure(_, None))
            ),
            binaryCodec.mapOrFail(
              transform.f.asInstanceOf[Any => Either[String, T]](_).left.map(DeserializationFailure(_, None))
            )
          )
        case Schema.Lazy(inner)                                                                          =>
          deriveInContext(inner())
        case map: Schema.Map[_, _]                                                                       =>
          codecs.mapCodec(deriveInContext(map.keySchema), deriveInContext(map.valueSchema)).asInstanceOf[BinaryCodec[T]]
        case set: Schema.Set[_]                                                                          =>
          codecs.setCodec(deriveInContext(set.elementSchema)).asInstanceOf[BinaryCodec[T]]
        case sequence: Schema.Sequence[_, _, _]                                                          =>
          val baseCodec = chunkCodec[Any](
            deriveInContext(sequence.elementSchema.asInstanceOf[Schema[Any]])
          )
          BinaryCodec.from(
            baseCodec.contramap(sequence.toChunk.asInstanceOf[T => Chunk[Any]]),
            baseCodec.map(sequence.fromChunk.asInstanceOf[Chunk[Any] => T])
          )
        case Schema.Primitive(standardType, _)                                                           =>
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

        case Schema.Optional(schema, _)    =>
          codecs.optionCodec(deriveInContext(schema)).asInstanceOf[BinaryCodec[T]]
        case Schema.Fail(message, _)       =>
          BinaryCodec.from(
            (_: T) => failSerializerWith(SerializationFailure(message, None)),
            () => failDeserializerWith[T](DeserializationFailure(message, None))
          )
        case Schema.Tuple2(left, right, _) =>
          deriveTuple(left.asInstanceOf[Schema[Any]], right.asInstanceOf[Schema[Any]])
        case Schema.Either(left, right, _) =>
          codecs
            .eitherCodec(
              deriveInContext(left),
              deriveInContext(right)
            )
            .asInstanceOf[BinaryCodec[T]]
        case Schema.Dynamic(_)             =>
          dynamicValueCodec.asInstanceOf[BinaryCodec[T]]
      }

  private def deriveRecord[T, S <: Schema.Record[_]](evolutionSteps: Vector[Evolution], schema: S)(implicit
      recordSerializer: RecordSerializer[S],
      recordDeserializer: RecordDeserializer[S]
  ): BinaryCodec[T] =
    new AdtCodec[T, RecordDeserializerBase.SchemaBuilderState](
      evolutionSteps = evolutionSteps,
      typeName = schema.id.name,
      constructors = Vector(schema.id.name),
      transientFields = getTransientFields(schema.fields),
      getSerializationCommands = (value: T) => recordSerializer.getSerializationCommands(schema, value),
      deserializationCommands = recordDeserializer.getDeserializationCommands(schema),
      initialBuilderState = RecordDeserializerBase.SchemaBuilderState.initial,
      materialize = builderState =>
        Unsafe.unsafe { implicit u =>
          schema
            .construct(builderState.asChunk)
            .map(_.asInstanceOf[T])
            .left
            .map(msg => DeserializationFailure(msg, None))
        }
    )

  private def deriveEnum[T, S <: Schema.Enum[_]](evolutionSteps: Vector[Evolution], schema: S)(implicit
      enumSerializer: EnumSerializer[S],
      enumDeserializer: EnumDeserializer[S]
  ): BinaryCodec[T] =
    new AdtCodec[T, EnumDeserializerBase.SchemaBuilderState](
      evolutionSteps = evolutionSteps,
      typeName = schema.id.name,
      constructors = schema.cases.map(_.id).toVector,
      transientFields = Map.empty,
      getSerializationCommands = (value: T) => enumSerializer.getSerializationCommands(schema, value),
      deserializationCommands = enumDeserializer.getDeserializationCommands(schema),
      initialBuilderState = EnumDeserializerBase.SchemaBuilderState.initial,
      materialize = builderState => Right(builderState.result.asInstanceOf[T])
    )

  private def deriveTuple[T](left: Schema[Any], right: Schema[Any])(implicit
      derivationContext: DerivationContext
  ): BinaryCodec[T] = {
    val fields = ChunkBuilder.make[Schema[Any]](2)
    gatherTupleFields(left, fields)
    fields += right
    deriveNTuple(fields.result().reverse)
  }

  @tailrec
  private def gatherTupleFields(schema: Schema[Any], fields: ChunkBuilder[Schema[Any]]): Unit =
    schema.asInstanceOf[Schema[_]] match {
      case Schema.Tuple2(left, right, _) =>
        fields += right.asInstanceOf[Schema[Any]]
        gatherTupleFields(left.asInstanceOf[Schema[Any]], fields)
      case _                             =>
        fields += schema
    }

  private def deriveNTuple[T](
      values: Chunk[Schema[Any]]
  )(implicit derivationContext: DerivationContext): BinaryCodec[T] =
    values.length match {
      case 2 =>
        codecs
          .tuple2Codec(
            deriveInContext(values(0)),
            fieldReaderFor(values(0)),
            deriveInContext(values(1)),
            fieldReaderFor(values(1))
          )
          .asInstanceOf[BinaryCodec[T]]
    }

  private def fieldReaderFor(
      schema: Schema[_]
  )(implicit derivationContext: DerivationContext): codecs.TupleFieldReader[Any] =
    RecordDeserializer.findTopLevelOptionalNode(schema) match {
      case Some(optional) =>
        codecs.TupleFieldReader
          .optionalFieldReader[Any](deriveInContext(optional.schema.asInstanceOf[Schema[Any]]))
          .asInstanceOf[codecs.TupleFieldReader[Any]]
      case None           =>
        codecs.TupleFieldReader.requiredFieldReader[Any](deriveInContext(schema.asInstanceOf[Schema[Any]]))
    }

  private def getTransientFields(fields: Chunk[Schema.Field[_, _]]): Map[Symbol, Any] =
    fields.flatMap { field =>
      field.annotations.collect { case transientField(defaultValue) =>
        Symbol(field.name) -> defaultValue
      }
    }.toMap

  private def getEvolutionStepsFromAnnotation(value: Chunk[Any]): Vector[Evolution] =
    value.collectFirst { case ann: evolutionSteps => ann } match {
      case None             => Vector(InitialVersion)
      case Some(annotation) => InitialVersion +: annotation.steps.toVector
    }
}
