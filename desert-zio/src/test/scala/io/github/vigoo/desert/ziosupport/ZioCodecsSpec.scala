package io.github.vigoo.desert.ziosupport

import io.github.vigoo.desert.codecs._
import io.github.vigoo.desert.ziosupport.codecs._
import io.github.vigoo.desert.ziosupport.syntax._
import zio.Chunk
import zio.test.Assertion._
import zio.test._

object ZioCodecsSpec extends ZIOSpecDefault {
  override def spec: Spec[TestEnvironment, Any] =
    suite("ZIO serialization codecs")(
      test("correctly byte chunks") {
        val chunkIn = Chunk[Byte](1, 2, 4, 8, 16)
        for {
          serialized <- serializeToArray(chunkIn)
          chunkOut   <- deserializeFromArray[Chunk[Byte]](serialized)
        } yield assert(chunkIn)(equalTo(chunkOut))
      },
      test("correctly int chunks") {
        val chunkIn = Chunk[Int](1, 2, 4, 8, 16)
        for {
          serialized <- serializeToArray(chunkIn)
          chunkOut   <- deserializeFromArray[Chunk[Int]](serialized)
        } yield assert(chunkIn)(equalTo(chunkOut))
      }
    )
}
