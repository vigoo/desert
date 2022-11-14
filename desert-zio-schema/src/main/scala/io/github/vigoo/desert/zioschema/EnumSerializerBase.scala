package io.github.vigoo.desert.zioschema

import io.github.vigoo.desert.{AdtCodec, BinaryCodec, SerializingTransientConstructor, transientConstructor}
import zio.schema.{CaseSet, Schema}

private[zioschema] trait EnumSerializerBase {

  implicit def enumNSerializer[Z, C <: CaseSet.Aux[Z]](implicit
      derivationContext: DerivationContext
  ): EnumSerializer[Schema.EnumN[Z, C]] =
    (schema: Schema.EnumN[Z, C], value: Any) => serializeCases(value)(schema.cases: _*)

  protected def serializeCases(
      value: Any
  )(cases: Schema.Case[_, _]*)(implicit derivationContext: DerivationContext): List[AdtCodec.SerializationCommand] =
    cases.find(_.asInstanceOf[Schema.Case[Any, Any]].deconstructOption(value).isDefined) match {
      case Some(matchingCase) =>
        val isTransient = matchingCase.annotations.contains(transientConstructor())
        if (isTransient)
          List(AdtCodec.SerializationCommand.Fail(SerializingTransientConstructor(matchingCase.id)))
        else
          List(
            AdtCodec.SerializationCommand.WriteConstructor(
              constructorName = matchingCase.id,
              value = matchingCase.asInstanceOf[Schema.Case[Any, Any]].deconstruct(value),
              () => DerivedBinaryCodec.deriveInContext(matchingCase.schema).asInstanceOf[BinaryCodec[Any]]
            )
          )

      case None => List.empty
    }
}
