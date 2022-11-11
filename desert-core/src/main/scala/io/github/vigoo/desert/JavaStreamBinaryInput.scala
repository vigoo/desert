package io.github.vigoo.desert

import java.io.{DataInputStream, EOFException, InputStream}

import scala.util.{Failure, Success, Try}

class JavaStreamBinaryInput(stream: InputStream) extends BinaryInput {
  private val dataStream = new DataInputStream(stream)

  override final def readByte(): Either[DesertFailure, Byte] =
    handleFailures(dataStream.readByte())

  override final def readShort(): Either[DesertFailure, Short] =
    handleFailures(dataStream.readShort())

  override final def readInt(): Either[DesertFailure, Int] =
    handleFailures(dataStream.readInt())

  override final def readLong(): Either[DesertFailure, Long] =
    handleFailures(dataStream.readLong())

  override def readFloat(): Either[DesertFailure, Float] =
    handleFailures(dataStream.readFloat())

  override def readDouble(): Either[DesertFailure, Double] =
    handleFailures(dataStream.readDouble())

  override final def readBytes(count: Int): Either[DesertFailure, Array[Byte]] = {
    val buffer = new Array[Byte](count)
    if (count == 0) {
      Right(buffer)
    } else {
      for {
        readBytes <- handleFailures(dataStream.read(buffer))
        _         <- assert(readBytes == count, InputEndedUnexpectedly())
      } yield buffer
    }
  }

  private def handleFailures[T](f: => T): Either[DesertFailure, T] =
    Try(f) match {
      case Success(value)                => Right(value)
      case Failure(reason: EOFException) => Left(InputEndedUnexpectedly())
      case Failure(reason)               => Left(FailedToReadInput(reason))
    }

  private def assert(condition: Boolean, failure: DesertFailure): Either[DesertFailure, Unit] =
    if (condition) {
      Right(())
    } else {
      Left(failure)
    }
}
