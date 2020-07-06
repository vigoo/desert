package io.github.vigoo.desert

import java.io.{DataInputStream, EOFException, InputStream}

import zio.ZIO

class JavaStreamBinaryInput(stream: InputStream) extends BinaryInput {
  private val dataStream = new DataInputStream(stream)

  override final def readByte(): ZIO[Any, DesertFailure, Byte] =
    handleFailures(dataStream.readByte())

  override final def readShort(): ZIO[Any, DesertFailure, Short] =
    handleFailures(dataStream.readShort())

  override final def readInt(): ZIO[Any, DesertFailure, Int] =
    handleFailures(dataStream.readInt())

  override final def readLong(): ZIO[Any, DesertFailure, Long] =
    handleFailures(dataStream.readLong())

  override def readFloat(): ZIO[Any, DesertFailure, Float] =
    handleFailures(dataStream.readFloat())

  override def readDouble(): ZIO[Any, DesertFailure, Double] =
    handleFailures(dataStream.readDouble())

  override final def readBytes(count: Int): ZIO[Any, DesertFailure, Array[Byte]] = {
    val buffer = new Array[Byte](count)
    if (count == 0) {
      ZIO.succeed(buffer)
    } else {
      for {
        readBytes <- handleFailures(dataStream.read(buffer))
        _ <- assert(readBytes == count, InputEndedUnexpectedly())
      } yield buffer
    }
  }

  private def handleFailures[T](f: => T): ZIO[Any, DesertFailure, T] =
    ZIO.effect(f).mapError {
      case _: EOFException => InputEndedUnexpectedly()
      case reason: Throwable => FailedToReadInput(reason)
    }

  private def assert(condition: => Boolean, failure: DesertFailure): ZIO[Any, DesertFailure, Unit] =
    if (condition) {
      ZIO.unit
    } else {
      ZIO.fail(failure)
    }
}
