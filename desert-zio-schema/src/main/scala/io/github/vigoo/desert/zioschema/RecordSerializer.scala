package io.github.vigoo.desert.zioschema

import io.github.vigoo.desert.{AdtCodec, BinaryCodec}
import zio.schema.Schema

import scala.collection.immutable.ListMap

private[zioschema] trait RecordSerializer[S <: Schema.Record[_]] {
  def getSerializationCommands(schema: S, value: Any): List[AdtCodec.SerializationCommand]
}

private[zioschema] object RecordSerializer {
  // TODO: should get some kind of derivation context to reuse derived codecs

  implicit val genericRecordSerializer: RecordSerializer[Schema.GenericRecord] =
    (schema: Schema.GenericRecord, value: Any) =>
      schema.fieldSet.toChunk.map { field =>
        AdtCodec.SerializationCommand.WriteField[Any](
          field.label,
          value.asInstanceOf[ListMap[String, _]](field.label),
          () => DerivedBinaryCodec.derive(field.schema.asInstanceOf[Schema[Any]])
        )
      }.toList

  implicit def caseClass0Serializer[Z]: RecordSerializer[Schema.CaseClass0[Z]] =
    (_: Schema.CaseClass0[Z], _: Any) => Nil

  implicit def caseClass1Serializer[A, Z]: RecordSerializer[Schema.CaseClass1[A, Z]] =
    (schema: Schema.CaseClass1[A, Z], value: Any) =>
      List(
        AdtCodec.SerializationCommand.WriteField(
          schema.field.label,
          schema.asInstanceOf[Schema.CaseClass1[Any, Any]].extractField(value),
          () => DerivedBinaryCodec.derive(schema.field.schema.asInstanceOf[Schema[Any]])
        )
      )

  implicit def caseClass2Serializer[A1, A2, Z]: RecordSerializer[Schema.CaseClass2[A1, A2, Z]] =
    (schema: Schema.CaseClass2[A1, A2, Z], value: Any) =>
      List(
        AdtCodec.SerializationCommand.WriteField(
          schema.field1.label,
          schema.asInstanceOf[Schema.CaseClass2[Any, Any, Any]].extractField1(value),
          () => DerivedBinaryCodec.derive(schema.field1.schema.asInstanceOf[Schema[Any]])
        ),
        AdtCodec.SerializationCommand.WriteField(
          schema.field2.label,
          schema.asInstanceOf[Schema.CaseClass2[Any, Any, Any]].extractField2(value),
          () => DerivedBinaryCodec.derive(schema.field2.schema.asInstanceOf[Schema[Any]])
        )
      )
}
