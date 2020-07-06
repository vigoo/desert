package io.github.vigoo.desert

import java.io.{DataOutputStream, OutputStream}

import zio.ZIO

class JavaStreamBinaryOutput(stream: OutputStream) extends BinaryOutput {
  private val dataStream = new DataOutputStream(stream)

  override final def writeByte(value: Byte): ZIO[Any, DesertFailure, Unit] =
    handleFailures(dataStream.writeByte(value))

  override final def writeShort(value: Short): ZIO[Any, DesertFailure, Unit] =
    handleFailures(dataStream.writeShort(value))

  override final def writeInt(value: Int): ZIO[Any, DesertFailure, Unit] =
    handleFailures(dataStream.writeInt(value))

  override final def writeLong(value: Long): ZIO[Any, DesertFailure, Unit] =
    handleFailures(dataStream.writeLong(value))

  override def writeFloat(value: Float): ZIO[Any, DesertFailure, Unit] =
    handleFailures(dataStream.writeFloat(value))

  override def writeDouble(value: Double): ZIO[Any, DesertFailure, Unit] =
    handleFailures(dataStream.writeDouble(value))

  override final def writeBytes(value: Array[Byte]): ZIO[Any, DesertFailure, Unit] =
    handleFailures(dataStream.write(value))

  private def handleFailures[T](f: => T): ZIO[Any, DesertFailure, T] =
    ZIO.effect(f).mapError(FailedToWriteOutput.apply)
}
