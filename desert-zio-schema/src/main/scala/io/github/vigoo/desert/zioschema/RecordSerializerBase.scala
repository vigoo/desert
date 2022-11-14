package io.github.vigoo.desert.zioschema

import io.github.vigoo.desert.AdtCodec
import zio.schema.Schema

import scala.collection.immutable.ListMap

private[zioschema] trait RecordSerializerBase {

  implicit val genericRecordSerializer: RecordSerializer[Schema.GenericRecord] =
    (schema: Schema.GenericRecord, value: Any) =>
      schema.fieldSet.toChunk.map { field =>
        AdtCodec.SerializationCommand.WriteField[Any](
          field.name,
          value.asInstanceOf[ListMap[String, _]](field.name),
          () => DerivedBinaryCodec.derive(field.schema.asInstanceOf[Schema[Any]])
        )
      }.toList

  implicit def caseClass0Serializer[Z]: RecordSerializer[Schema.CaseClass0[Z]] =
    (_: Schema.CaseClass0[Z], _: Any) => Nil
}
