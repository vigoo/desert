package io.github.vigoo.desert.zioschema

import io.github.vigoo.desert.AdtCodec
import zio.schema.Schema

import scala.collection.immutable.ListMap

private[zioschema] trait RecordSerializer[S <: Schema.Record[_]] {
  def getSerializationCommands(schema: S, value: Any): List[AdtCodec.SerializationCommand]
}

private[zioschema] object RecordSerializer {

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

  implicit def caseClass1Serializer[A, Z]: RecordSerializer[Schema.CaseClass1[A, Z]] =
    (schema: Schema.CaseClass1[A, Z], value: Any) =>
      List(
        AdtCodec.SerializationCommand.WriteField(
          schema.field.name,
          schema.asInstanceOf[Schema.CaseClass1[Any, Any]].field.get(value),
          () => DerivedBinaryCodec.derive(schema.field.schema.asInstanceOf[Schema[Any]])
        )
      )

  implicit def caseClass2Serializer[A1, A2, Z](implicit
      derivationContext: DerivationContext
  ): RecordSerializer[Schema.CaseClass2[A1, A2, Z]] =
    (schema: Schema.CaseClass2[A1, A2, Z], value: Any) =>
      List(
        AdtCodec.SerializationCommand.WriteField(
          schema.field1.name,
          schema.asInstanceOf[Schema.CaseClass2[Any, Any, Any]].field1.get(value),
          () => DerivedBinaryCodec.deriveInContext(schema.field1.schema.asInstanceOf[Schema[Any]])
        ),
        AdtCodec.SerializationCommand.WriteField(
          schema.field2.name,
          schema.asInstanceOf[Schema.CaseClass2[Any, Any, Any]].field2.get(value),
          () => DerivedBinaryCodec.deriveInContext(schema.field2.schema.asInstanceOf[Schema[Any]])
        )
      )

  implicit def caseClass3Serializer[A1, A2, A3, Z](implicit
      derivationContext: DerivationContext
  ): RecordSerializer[Schema.CaseClass3[A1, A2, A3, Z]] =
    (schema: Schema.CaseClass3[A1, A2, A3, Z], value: Any) =>
      List(
        AdtCodec.SerializationCommand.WriteField(
          schema.field1.name,
          schema.asInstanceOf[Schema.CaseClass3[Any, Any, Any, Any]].field1.get(value),
          () => DerivedBinaryCodec.deriveInContext(schema.field1.schema.asInstanceOf[Schema[Any]])
        ),
        AdtCodec.SerializationCommand.WriteField(
          schema.field2.name,
          schema.asInstanceOf[Schema.CaseClass3[Any, Any, Any, Any]].field2.get(value),
          () => DerivedBinaryCodec.deriveInContext(schema.field2.schema.asInstanceOf[Schema[Any]])
        ),
        AdtCodec.SerializationCommand.WriteField(
          schema.field3.name,
          schema.asInstanceOf[Schema.CaseClass3[Any, Any, Any, Any]].field3.get(value),
          () => DerivedBinaryCodec.deriveInContext(schema.field3.schema.asInstanceOf[Schema[Any]])
        )
      )
}
