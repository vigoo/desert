package io.github.vigoo.desert.zio
import io.github.vigoo.desert.BinarySerializer.Ser
import io.github.vigoo.desert.syntax._
import zio.Chunk

trait ZioBinarySerializerOps {
  final def writeByteChunk[A](chunk: Chunk[Byte]): Ser[Unit] = getOutput.flatMap { output =>
    Ser.fromEither(output.writeBytes(chunk.toArray))
  }
}

object ZioBinarySerializerOps extends ZioBinarySerializerOps
