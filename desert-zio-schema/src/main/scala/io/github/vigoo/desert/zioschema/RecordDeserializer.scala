package io.github.vigoo.desert.zioschema

import io.github.vigoo.desert.{AdtCodec, BinaryCodec, transientField}
import io.github.vigoo.desert.zioschema.RecordDeserializer.SchemaBuilderState
import zio.Chunk
import zio.schema.Schema

import scala.annotation.tailrec

private[zioschema] trait RecordDeserializer[S <: Schema.Record[_]] {
  def getDeserializationCommands(schema: S): List[AdtCodec.DeserializationCommand[SchemaBuilderState]]
}

private[zioschema] object RecordDeserializer {
  final case class SchemaBuilderState(fields: List[Any]) {
    def storeField(value: Any): SchemaBuilderState =
      this.copy(fields = value :: fields)

    def asChunk: Chunk[Any] = Chunk.fromIterable(fields).reverse
  }

  object SchemaBuilderState {
    val initial: SchemaBuilderState = SchemaBuilderState(List.empty)
  }

  implicit val genericRecordDeserializer: RecordDeserializer[Schema.GenericRecord] =
    (schema: Schema.GenericRecord) =>
      schema.fieldSet.toChunk.toList.map { field =>
        fieldToDeserializationCommand(field)
      }

  implicit def caseClass0Deserializer[Z]: RecordDeserializer[Schema.CaseClass0[Z]] =
    (schema: Schema.CaseClass0[Z]) => List.empty

  implicit def caseClass1Deserializer[A1, Z]: RecordDeserializer[Schema.CaseClass1[A1, Z]] =
    (schema: Schema.CaseClass1[A1, Z]) =>
      List(
        fieldToDeserializationCommand(schema.field)
      )

  implicit def caseClass2Deserializer[A1, A2, Z]: RecordDeserializer[Schema.CaseClass2[A1, A2, Z]] =
    (schema: Schema.CaseClass2[A1, A2, Z]) =>
      List(
        fieldToDeserializationCommand(schema.field1),
        fieldToDeserializationCommand(schema.field2)
      )

  implicit def caseClass3Deserializer[A1, A2, A3, Z]: RecordDeserializer[Schema.CaseClass3[A1, A2, A3, Z]] =
    (schema: Schema.CaseClass3[A1, A2, A3, Z]) =>
      List(
        fieldToDeserializationCommand(schema.field1),
        fieldToDeserializationCommand(schema.field2),
        fieldToDeserializationCommand(schema.field3)
      )

  private def fieldToDeserializationCommand(
      field: Schema.Field[_]
  ): AdtCodec.DeserializationCommand[SchemaBuilderState] = {
    val optional            = findTopLevelOptionalNode(field.schema)
    val transientAnnotation = field.annotations.collectFirst { case tf: transientField =>
      tf
    }

    if (transientAnnotation.isDefined)
      AdtCodec.DeserializationCommand.ReadTransient(
        field.label,
        (value: Any, state: SchemaBuilderState) => state.storeField(value)
      )
    else if (optional.isDefined) {
      AdtCodec.DeserializationCommand.ReadOptional(
        field.label,
        () => DerivedBinaryCodec.derive(optional.get.codec.asInstanceOf[Schema[Any]]),
        () => DerivedBinaryCodec.derive(field.schema).asInstanceOf[BinaryCodec[Option[Any]]],
        (value: Option[Any], state: SchemaBuilderState) => state.storeField(value)
      )
    } else
      AdtCodec.DeserializationCommand.Read(
        field.label,
        () => DerivedBinaryCodec.derive(field.schema.asInstanceOf[Schema[Any]]),
        (value: Any, state: SchemaBuilderState) => state.storeField(value)
      )
  }

  @tailrec
  private[zioschema] def findTopLevelOptionalNode(value: Schema[_]): Option[Schema.Optional[_]] =
    value match {
      case enum: Schema.Enum[_]                                 => None
      case record: Schema.Record[_]                             => None
      case collection: Schema.Collection[_, _]                  => None
      case Schema.Transform(codec, f, g, annotations, identity) =>
        findTopLevelOptionalNode(codec)
      case Schema.Primitive(standardType, annotations)          => None
      case s @ Schema.Optional(codec, annotations)              => Some(s)
      case Schema.Fail(message, annotations)                    => None
      case Schema.Tuple(left, right, annotations)               => None
      case Schema.EitherSchema(left, right, annotations)        => None
      case Schema.Lazy(schema0)                                 => findTopLevelOptionalNode(schema0())
      case Schema.Meta(ast, annotations)                        => None
      case Schema.Dynamic(annotations)                          => None
      case Schema.SemiDynamic(defaultValue, annotations)        => None
    }
}
