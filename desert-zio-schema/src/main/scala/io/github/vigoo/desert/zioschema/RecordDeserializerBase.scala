package io.github.vigoo.desert.zioschema

import io.github.vigoo.desert.{AdtCodec, BinaryCodec, transientField}
import io.github.vigoo.desert.zioschema.RecordDeserializerBase.SchemaBuilderState
import zio.Chunk
import zio.schema.Schema

import scala.annotation.tailrec

private[zioschema] trait RecordDeserializerBase {
  implicit def genericRecordDeserializer(implicit
      derivationContext: DerivationContext
  ): RecordDeserializer[Schema.GenericRecord] =
    (schema: Schema.GenericRecord) =>
      schema.fieldSet.toChunk.toList.map { field =>
        fieldToDeserializationCommand(field)
      }

  implicit def caseClass0Deserializer[Z]: RecordDeserializer[Schema.CaseClass0[Z]] =
    (_: Schema.CaseClass0[Z]) => List.empty

  protected def fieldToDeserializationCommand(
      field: Schema.Field[_, _]
  )(implicit derivationContext: DerivationContext): AdtCodec.DeserializationCommand[SchemaBuilderState] = {
    val optional            = findTopLevelOptionalNode(field.schema)
    val transientAnnotation = field.annotations.collectFirst { case tf: transientField =>
      tf
    }

    if (transientAnnotation.isDefined)
      AdtCodec.DeserializationCommand.ReadTransient(
        field.name,
        (value: Any, state: SchemaBuilderState) => state.storeField(value)
      )
    else if (optional.isDefined) {
      AdtCodec.DeserializationCommand.ReadOptional(
        field.name,
        () => DerivedBinaryCodec.deriveInContext(optional.get.schema.asInstanceOf[Schema[Any]]),
        () => DerivedBinaryCodec.deriveInContext(field.schema).asInstanceOf[BinaryCodec[Option[Any]]],
        (value: Option[Any], state: SchemaBuilderState) => state.storeField(value)
      )
    } else
      AdtCodec.DeserializationCommand.Read(
        field.name,
        () => DerivedBinaryCodec.deriveInContext(field.schema.asInstanceOf[Schema[Any]]),
        (value: Any, state: SchemaBuilderState) => state.storeField(value)
      )
  }

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
}

private[zioschema] object RecordDeserializerBase {
  final case class SchemaBuilderState(fields: List[Any]) {
    def storeField(value: Any): SchemaBuilderState =
      this.copy(fields = value :: fields)

    def asChunk: Chunk[Any] = Chunk.fromIterable(fields).reverse
  }

  object SchemaBuilderState {
    val initial: SchemaBuilderState = SchemaBuilderState(List.empty)
  }
}
