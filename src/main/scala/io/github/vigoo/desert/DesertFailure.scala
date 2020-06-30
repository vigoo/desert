package io.github.vigoo.desert

import io.github.vigoo.desert.SerializerState.StringId
import io.github.vigoo.desert.TypeRegistry.RegisteredTypeId

sealed trait DesertFailure
case class FailedToReadInput(reason: Throwable) extends DesertFailure
case class InputEndedUnexpectedly() extends DesertFailure
case class FailedToWriteOutput(reason: Throwable) extends DesertFailure
case class DeserializationFailure(message: String, reason: Option[Throwable]) extends DesertFailure
case class DeserializingNonExistingChunk(chunk: Byte) extends DesertFailure
case class NonOptionalFieldSerializedAsNone(fieldName: String) extends DesertFailure
case class UnknownFieldReferenceInEvolutionStep(name: String) extends DesertFailure
case class UnknownSerializedEvolutionStep(code: Int) extends DesertFailure
case class InvalidStringId(id: StringId) extends DesertFailure
case class FieldRemovedInSerializedVersion(fieldName: String) extends DesertFailure
case class FieldWithoutDefaultValuesIsMissing(fieldName: String) extends DesertFailure
case class TypeNotRegistered(cls: Class[_]) extends DesertFailure
case class InvalidTypeId(id: RegisteredTypeId) extends DesertFailure
case class SerializationUpcastError(valueType: Class[_], targetType: Class[_], reason: Throwable) extends DesertFailure