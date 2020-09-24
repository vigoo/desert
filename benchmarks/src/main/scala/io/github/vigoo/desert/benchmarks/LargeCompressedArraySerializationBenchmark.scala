package io.github.vigoo.desert.benchmarks

import java.nio.charset.StandardCharsets
import java.util.concurrent.TimeUnit
import java.util.zip.Deflater

import io.github.vigoo.desert.BinaryCodec
import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Measurement, Mode, OutputTimeUnit, Param, Scope, Setup, State, Warmup}
import zio.BootstrapRuntime
import io.github.vigoo.desert.syntax._

import scala.io.Source

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
class LargeCompressedArraySerializationBenchmark extends BootstrapRuntime {
  import LargeCompressedArraySerializationBenchmark._

  var testData: TestData = _
  var serializedData: Array[Byte] = _

  @Param(Array("1"))
  var level: Int = Deflater.DEFAULT_COMPRESSION

  @Setup
  def loadData(): Unit = {
    testData = TestData(Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("testdata/data")).mkString, level)

//    for (l <- (Deflater.BEST_SPEED to Deflater.BEST_COMPRESSION)) {
//      val bs = serializeToArray(testData.copy(level = l)).toOption.get
//      println(s"Level $l: ${bs.length} bytes")
//    }
//    println(s"Uncompressed size: ${testData.data.getBytes(StandardCharsets.UTF_8).length}")

    serializedData = serializeToArray(testData).toOption.get

    println(s"Using compression level $level")
  }

  @Benchmark
  def serializeLargeCompressedArray(): Unit = {
    val data = serializeToArray(testData)
  }

  @Benchmark
  def deserializeLargeCompressedArray(): Unit = {
    val data = deserializeFromArray[TestData](serializedData)
  }
}

object LargeCompressedArraySerializationBenchmark {

  case class TestData(data: String, level: Int)

  object TestData {
    implicit val binaryCodec: BinaryCodec[TestData] = BinaryCodec.define(
      (data: TestData) => writeCompressedBytes(data.data.getBytes(StandardCharsets.UTF_8), data.level)
    )(readCompressedByteArray().map(bytes => TestData(new String(bytes, StandardCharsets.UTF_8), Deflater.BEST_SPEED)))
  }
}