package io.github.vigoo.desert.zioschema

import io.github.vigoo.desert.{AdtCodec, BinaryCodec, transientConstructor}
import io.github.vigoo.desert.zioschema.EnumDeserializer.SchemaBuilderState
import zio.schema.Schema

private[zioschema] trait EnumDeserializer[S <: Schema.Enum[_]] {
  def getDeserializationCommands(schema: S): List[AdtCodec.DeserializationCommand[SchemaBuilderState]]
}

private[zioschema] object EnumDeserializer {
  final case class SchemaBuilderState(result: Any)
  object SchemaBuilderState {
    val initial: SchemaBuilderState = SchemaBuilderState(null)
  }

  implicit def enum2Deserializer[A1 <: Z, A2 <: Z, Z](implicit
      derivationContext: DerivationContext
  ): EnumDeserializer[Schema.Enum2[A1, A2, Z]] =
    new EnumDeserializer[Schema.Enum2[A1, A2, Z]] {
      override def getDeserializationCommands(
          schema: Schema.Enum2[A1, A2, Z]
      ): List[AdtCodec.DeserializationCommand[SchemaBuilderState]] =
        List(schema.case1, schema.case2)
          .filterNot(isTransient)
          .map(caseToDeserializationCommand)
    }

  implicit def enum3Deserializer[A1 <: Z, A2 <: Z, A3 <: Z, Z](implicit
      derivationContext: DerivationContext
  ): EnumDeserializer[Schema.Enum3[A1, A2, A3, Z]] =
    new EnumDeserializer[Schema.Enum3[A1, A2, A3, Z]] {
      override def getDeserializationCommands(
          schema: Schema.Enum3[A1, A2, A3, Z]
      ): List[AdtCodec.DeserializationCommand[SchemaBuilderState]] =
        List(schema.case1, schema.case2, schema.case3)
          .filterNot(isTransient)
          .map(caseToDeserializationCommand)
    }

  private def caseToDeserializationCommand(
      c: Schema.Case[_, _]
  )(implicit derivationContext: DerivationContext): AdtCodec.DeserializationCommand[SchemaBuilderState] =
    AdtCodec.DeserializationCommand.ReadConstructor(
      c.id,
      () => DerivedBinaryCodec.deriveInContext(c.codec).asInstanceOf[BinaryCodec[Any]],
      (value: Any, state) =>
        state.copy(result = value)
    )

  private def isTransient(c: Schema.Case[_, _]): Boolean =
    c.annotations.contains(transientConstructor())
}
