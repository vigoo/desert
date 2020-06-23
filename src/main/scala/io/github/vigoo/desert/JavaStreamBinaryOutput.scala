package io.github.vigoo.desert

import java.io.{DataOutputStream, OutputStream}

import scala.util.{Failure, Success, Try}

class JavaStreamBinaryOutput(stream: OutputStream) extends BinaryOutput {
  private val dataStream = new DataOutputStream(stream)

  override final def writeByte(value: Byte): Either[DesertFailure, Unit] =
    handleFailures(dataStream.writeByte(value))

  override final def writeShort(value: Short): Either[DesertFailure, Unit] =
    handleFailures(dataStream.writeShort(value))

  override final def writeInt(value: Int): Either[DesertFailure, Unit] =
    handleFailures(dataStream.writeInt(value))

  override final def writeLong(value: Long): Either[DesertFailure, Unit] =
    handleFailures(dataStream.writeLong(value))

  override final def writeBytes(value: Array[Byte]): Either[DesertFailure, Unit] =
    handleFailures(dataStream.write(value))

  private def handleFailures[T](f: => T): Either[DesertFailure, Unit] =
    Try(f) match {
      case Success(value) => Right(value)
      case Failure(reason) => Left(FailedToWriteOutput(reason))
    }
}
