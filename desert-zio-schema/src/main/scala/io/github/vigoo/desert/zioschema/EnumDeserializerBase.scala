package io.github.vigoo.desert.zioschema

import io.github.vigoo.desert.{AdtCodec, BinaryCodec, transientConstructor}
import zio.schema.{CaseSet, Schema}

private[zioschema] trait EnumDeserializerBase {

  implicit def enumNDeserializer[Z, C <: CaseSet.Aux[Z]](implicit
      derivationContext: DerivationContext
  ): EnumDeserializer[Schema.EnumN[Z, C]] =
    (schema: Schema.EnumN[Z, C]) => schema.cases.toList.filterNot(isTransient).map(caseToDeserializationCommand)

  protected def caseToDeserializationCommand(
      c: Schema.Case[_, _]
  )(implicit
      derivationContext: DerivationContext
  ): AdtCodec.DeserializationCommand[EnumDeserializerBase.SchemaBuilderState] =
    AdtCodec.DeserializationCommand.ReadConstructor(
      c.id,
      () => DerivedBinaryCodec.deriveInContext(c.schema).asInstanceOf[BinaryCodec[Any]],
      (value: Any, state) => state.copy(result = value)
    )

  protected def isTransient(c: Schema.Case[_, _]): Boolean =
    c.annotations.contains(transientConstructor())
}

private[zioschema] object EnumDeserializerBase {
  final case class SchemaBuilderState(result: Any)

  object SchemaBuilderState {
    val initial: SchemaBuilderState = SchemaBuilderState(null)
  }
}
