package io.github.vigoo.desert

trait BinaryInput {
  def readByte(): Either[DesertFailure, Byte]
  def readShort(): Either[DesertFailure, Short]
  def readInt(): Either[DesertFailure, Int]
  def readLong(): Either[DesertFailure, Long]
  def readBytes(count: Int): Either[DesertFailure, Array[Byte]]

  def read[T: BinaryDeserializer](): Either[DesertFailure, T] =
    implicitly[BinaryDeserializer[T]].deserialize(this)
}
