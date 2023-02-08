package io.github.vigoo.desert.golden

import io.github.vigoo.clipp._
import io.github.vigoo.clipp.parsers._
import io.github.vigoo.clipp.syntax._
import io.github.vigoo.clipp.zioapi._
import io.github.vigoo.desert._
import io.github.vigoo.desert.syntax._
import io.github.vigoo.desert.codecs._
import io.github.vigoo.desert.zio.syntax._
import _root_.zio._
import _root_.zio.nio.file.Files
import _root_.zio.nio.file.{Path => ZPath}

import java.io.IOException
import java.nio.file.Path

object Main extends ZIOAppDefault {
  final case class Params(targetDir: Path)

  def run: ZIO[ZIOAppArgs, Any, ExitCode] = {
    val paramSpec = for {
      _         <- metadata("golden-dataset-generator")
      targetDir <- parameter[Path]("target directory to put the generated binaries in", "TARGETDIR")
    } yield Params(targetDir)

    val program =
      for {
        params <- parameters[Params]

        dataset1 <- serializeToChunk(TestModel1.value1)
        _        <- saveChunkAs(params.targetDir, "dataset1.bin", dataset1)
      } yield ExitCode.success

    program
      .provideSome[ZIOAppArgs](
        parametersFromArgs(paramSpec).printUsageInfoOnFailure
      )
      .catchSome { _: ParserFailure => ZIO.succeed(ExitCode.failure) }
  }

  private def saveChunkAs(targetDir: Path, filename: String, data: Chunk[Byte]): ZIO[Any, IOException, Unit] =
    Files.writeBytes(ZPath.fromJava(targetDir) / filename, data)
}
