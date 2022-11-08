package io.github.vigoo.desert.zioschema

import io.github.vigoo.desert.{AdtCodec, BinaryCodec, SerializingTransientConstructor, transientConstructor}
import zio.schema.Schema

private[zioschema] trait EnumSerializer[S <: Schema.Enum[_]] {
  def getSerializationCommands(schema: S, value: Any): List[AdtCodec.SerializationCommand]
}

private[zioschema] object EnumSerializer {
  implicit def enum2Serializer[A1 <: Z, A2 <: Z, Z](implicit derivationContext: DerivationContext): EnumSerializer[Schema.Enum2[A1, A2, Z]] =
    (schema: Schema.Enum2[A1, A2, Z], value: Any) =>
      serializeCases(value)(schema.case1, schema.case2)

  implicit def enum3Serializer[A1 <: Z, A2 <: Z, A3 <: Z, Z](implicit derivationContext: DerivationContext): EnumSerializer[Schema.Enum3[A1, A2, A3, Z]] =
    (schema: Schema.Enum3[A1, A2, A3, Z], value: Any) =>
      serializeCases(value)(schema.case1, schema.case2, schema.case3)

  private def serializeCases(value: Any)(cases: Schema.Case[_, _]*)(implicit derivationContext: DerivationContext): List[AdtCodec.SerializationCommand] =
    cases.find(_.asInstanceOf[Schema.Case[Any, Any]].deconstruct(value).isDefined) match {
      case Some(matchingCase) =>
        val isTransient = matchingCase.annotations.contains(transientConstructor())
        if (isTransient)
          List(AdtCodec.SerializationCommand.Fail(SerializingTransientConstructor(matchingCase.id)))
        else
          List(AdtCodec.SerializationCommand.WriteConstructor(
            constructorName = matchingCase.id,
            value = matchingCase.asInstanceOf[Schema.Case[Any, Any]].unsafeDeconstruct(value),
            () => DerivedBinaryCodec.deriveInContext(matchingCase.codec).asInstanceOf[BinaryCodec[Any]]
          ))

      case None => List.empty
    }
}
