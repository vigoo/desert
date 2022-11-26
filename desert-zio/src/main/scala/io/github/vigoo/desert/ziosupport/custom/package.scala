package io.github.vigoo.desert.ziosupport

import io.github.vigoo.desert._
import io.github.vigoo.desert.custom.{getInput, getOutput}
import zio.Chunk

package object custom {
  final def writeByteChunk[A](chunk: Chunk[Byte]): Ser[Unit] = getOutput.flatMap { output =>
    Ser.fromEither(output.writeBytes(chunk.toArray))
  }

  final def readByteChunk[A](count: Int): Deser[Chunk[Byte]] = getInput.flatMap { input =>
    Deser.fromEither(input.readBytes(count).map(Chunk.fromArray[Byte]))
  }
}
