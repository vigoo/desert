package io.github.vigoo.desert

import io.github.vigoo.desert.SerializerState.StringId

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