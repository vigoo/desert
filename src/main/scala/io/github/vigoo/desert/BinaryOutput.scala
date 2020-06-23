package io.github.vigoo.desert

trait BinaryOutput {
  def writeByte(value: Byte): Either[DesertFailure, Unit]
  def writeShort(value: Short): Either[DesertFailure, Unit]
  def writeInt(value: Int): Either[DesertFailure, Unit]
  def writeLong(value: Long): Either[DesertFailure, Unit]
  def writeBytes(value: Array[Byte]): Either[DesertFailure, Unit]

  def writeUnknown(value: Any, typeRegistry: TypeRegistry): Either[DesertFailure, Unit] =
    ???

  def write[T : BinarySerializer](value: T): Either[DesertFailure, Unit] =
    implicitly[BinarySerializer[T]].serialize(value, this)
}
