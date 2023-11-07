package io.github.vigoo.desert.internal

import io.github.vigoo.desert.{BinaryInput, DesertException, DesertFailure}

import java.io.{DataInputStream, EOFException, InputStream}
import scala.util.control.NonFatal

class JavaStreamBinaryInput(stream: InputStream) extends BinaryInput {
  private val dataStream = new DataInputStream(stream)

  override final def readByte(): Byte =
    try dataStream.readByte()
    catch { case NonFatal(e) => throw new DesertException(wrapError(e)) }

  override final def readShort(): Short =
    try dataStream.readShort()
    catch { case NonFatal(e) => throw new DesertException(wrapError(e)) }

  override final def readInt(): Int =
    try dataStream.readInt()
    catch { case NonFatal(e) => throw new DesertException(wrapError(e)) }

  override final def readLong(): Long =
    try dataStream.readLong()
    catch { case NonFatal(e) => throw new DesertException(wrapError(e)) }

  override def readFloat(): Float =
    try dataStream.readFloat()
    catch { case NonFatal(e) => throw new DesertException(wrapError(e)) }

  override def readDouble(): Double =
    try dataStream.readDouble()
    catch { case NonFatal(e) => throw new DesertException(wrapError(e)) }

  override final def readBytes(count: Int): Array[Byte] = {
    val buffer = new Array[Byte](count)
    if (count > 0) {
      val readBytes =
        try dataStream.read(buffer)
        catch { case NonFatal(e) => throw new DesertException(wrapError(e)) }
      if (readBytes != count) {
        throw new DesertException(DesertFailure.InputEndedUnexpectedly())
      }
    }
    buffer
  }

  private def wrapError(reason: Throwable): DesertFailure =
    reason match {
      case _: EOFException => DesertFailure.InputEndedUnexpectedly()
      case _               => DesertFailure.FailedToReadInput(reason)
    }
}
