package io.github.vigoo.desert.internal

import io.github.vigoo.desert.{BinaryOutput, DesertException, DesertFailure}

import java.io.{DataOutputStream, OutputStream}
import scala.util.control.NonFatal

final class JavaStreamBinaryOutput(stream: OutputStream) extends BinaryOutput {
  private val buffer: Array[Byte] = new Array[Byte](4)
  private val dataStream          = new DataOutputStream(stream)

  override def writeByte(value: Byte): Unit =
    try dataStream.writeByte(value)
    catch { case NonFatal(e) => throw new DesertException(DesertFailure.FailedToWriteOutput(e)) }

  override def writeShort(value: Short): Unit =
    try dataStream.writeShort(value)
    catch { case NonFatal(e) => throw new DesertException(DesertFailure.FailedToWriteOutput(e)) }

  override def writeInt(value: Int): Unit = {
    buffer(0) = ((value >>> 24) & 0xff).toByte
    buffer(1) = ((value >>> 16) & 0xff).toByte
    buffer(2) = ((value >>> 8) & 0xff).toByte
    buffer(3) = ((value >>> 0) & 0xff).toByte
    try dataStream.write(buffer)
    catch { case NonFatal(e) => throw new DesertException(DesertFailure.FailedToWriteOutput(e)) }
  }

  override def writeLong(value: Long): Unit =
    try dataStream.writeLong(value)
    catch { case NonFatal(e) => throw new DesertException(DesertFailure.FailedToWriteOutput(e)) }

  override def writeFloat(value: Float): Unit =
    try dataStream.writeFloat(value)
    catch { case NonFatal(e) => throw new DesertException(DesertFailure.FailedToWriteOutput(e)) }

  override def writeDouble(value: Double): Unit =
    try dataStream.writeDouble(value)
    catch { case NonFatal(e) => throw new DesertException(DesertFailure.FailedToWriteOutput(e)) }

  override def writeBytes(value: Array[Byte]): Unit =
    try dataStream.write(value)
    catch { case NonFatal(e) => throw new DesertException(DesertFailure.FailedToWriteOutput(e)) }

  override def writeBytes(value: Array[Byte], start: Int, count: Int): Unit =
    try dataStream.write(value, start, count)
    catch { case NonFatal(e) => throw new DesertException(DesertFailure.FailedToWriteOutput(e)) }
}
