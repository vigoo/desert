package io.github.vigoo.desert

import io.github.vigoo.desert.SerializerState.{RefId, StringId}
import io.github.vigoo.desert.TypeRegistry.RegisteredTypeId

/** Desert failure type
  *
  * Every failure has a human-readable 'message' and an optional exception cause.
  */
sealed trait DesertFailure {

  /** Human readable failure message
    */
  def message: String

  /** Exception related to the failure, if any
    */
  def cause: Option[Throwable] = None
}
case class FailedToReadInput(reason: Throwable)                 extends DesertFailure {
  override def message: String          = "Failed to read input"
  override def cause: Option[Throwable] = Some(reason)
}
case class InputEndedUnexpectedly()                             extends DesertFailure {
  override def message: String = "Input ended unexpectedly"
}
case class FailedToWriteOutput(reason: Throwable)               extends DesertFailure {
  override def message: String          = "Failed to write output"
  override def cause: Option[Throwable] = Some(reason)
}
case class SerializationFailure(override val message: String, override val cause: Option[Throwable])
    extends DesertFailure
case class DeserializationFailure(override val message: String, override val cause: Option[Throwable])
    extends DesertFailure
case class DeserializingNonExistingChunk(chunk: Byte)           extends DesertFailure {
  override def message: String = s"Trying to deserialize a non-existing chunk ($chunk)"
}
case class NonOptionalFieldSerializedAsNone(fieldName: String)  extends DesertFailure {
  override def message: String = s"Non-optional field serialized as None ($fieldName)"
}
case class UnknownFieldReferenceInEvolutionStep(name: String)   extends DesertFailure {
  override def message: String = s"Unknown field reference in evolution step ($name)"
}
case class UnknownSerializedEvolutionStep(code: Int)            extends DesertFailure {
  override def message: String = s"Unknown serialized evolution step ($code)"
}
case class InvalidStringId(id: StringId)                        extends DesertFailure {
  override def message: String = s"Invalid string identifier ($id)"
}
case class FieldRemovedInSerializedVersion(fieldName: String)   extends DesertFailure {
  override def message: String = s"Field got removed in the serialized data ($fieldName)"
}
case class FieldWithoutDefaultValueIsMissing(fieldName: String) extends DesertFailure {
  override def message: String = s"Field without specified default value is missing ($fieldName)"
}
case class TypeNotRegistered(cls: Class[_])                     extends DesertFailure {
  override def message: String = s"Type is not registered (${cls.getName})"
}
case class InvalidTypeId(id: RegisteredTypeId)                  extends DesertFailure {
  override def message: String = s"Invalid type identifier (${id.value})"
}
case class SerializationUpcastError(valueType: Class[_], targetType: Class[_], reason: Throwable)
    extends DesertFailure {
  override def message: String          = s"Failed to upcast ${valueType.getName} to ${targetType.getName}"
  override def cause: Option[Throwable] = Some(reason)
}
case class InvalidConstructorName(name: String, typ: String)    extends DesertFailure {
  override def message: String = s"Invalid constructor name ($name) for type $typ"
}
case class InvalidConstructorId(id: Int, typ: String)           extends DesertFailure {
  override def message: String = s"Unknown constructor id ($id) for type $typ"
}
case class InvalidRefId(id: RefId)                              extends DesertFailure {
  override def message: String = s"Invalid reference identifier ($id)"
}
case class SerializingTransientConstructor(name: String)        extends DesertFailure {
  override def message: String = s"Trying to serialize a constructor marked as transient: $name"
}

/** Exception form of [[DesertFailure]] to be used in places where failure must be encoded by an exception (Future, Cats
  * Effect IO, etc).
  *
  * @param failure
  *   The failure to represent in the exception
  */
class DesertException(failure: DesertFailure) extends Exception(failure.message) {
  failure.cause.foreach(initCause)
}
