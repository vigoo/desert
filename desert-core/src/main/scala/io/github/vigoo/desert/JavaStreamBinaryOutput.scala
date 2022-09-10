package io.github.vigoo.desert

import java.io.{DataOutputStream, OutputStream}

import scala.util.{Failure, Success, Try}

final class JavaStreamBinaryOutput(stream: OutputStream) extends BinaryOutput {
  private val buffer: Array[Byte] = new Array[Byte](4)
  private val dataStream          = new DataOutputStream(stream)

  override def writeByte(value: Byte): Either[DesertFailure, Unit] =
    handleFailures(dataStream.writeByte(value))

  override def writeShort(value: Short): Either[DesertFailure, Unit] =
    handleFailures(dataStream.writeShort(value))

  override def writeInt(value: Int): Either[DesertFailure, Unit] = {
    buffer(0) = ((value >>> 24) & 0xff).toByte
    buffer(1) = ((value >>> 16) & 0xff).toByte
    buffer(2) = ((value >>> 8) & 0xff).toByte
    buffer(3) = ((value >>> 0) & 0xff).toByte
    handleFailures(dataStream.write(buffer))
  }

  override def writeLong(value: Long): Either[DesertFailure, Unit] =
    handleFailures(dataStream.writeLong(value))

  override def writeFloat(value: Float): Either[DesertFailure, Unit] =
    handleFailures(dataStream.writeFloat(value))

  override def writeDouble(value: Double): Either[DesertFailure, Unit] =
    handleFailures(dataStream.writeDouble(value))

  override def writeBytes(value: Array[Byte]): Either[DesertFailure, Unit] =
    handleFailures(dataStream.write(value))

  override def writeBytes(value: Array[Byte], start: Int, count: Int): Either[DesertFailure, Unit] =
    handleFailures(dataStream.write(value, start, count))

  private def handleFailures[T](f: => T): Either[DesertFailure, T] =
    Try(f) match {
      case Success(value)  => Right(value)
      case Failure(reason) => Left(FailedToWriteOutput(reason))
    }
}
