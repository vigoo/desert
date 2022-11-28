package io.github.vigoo.desert.benchmarks

import java.io.ByteArrayOutputStream
import java.util.concurrent.TimeUnit

import io.github.vigoo.desert.BinaryOutput
import io.github.vigoo.desert.internal.JavaStreamBinaryOutput
import org.openjdk.jmh.annotations.{
  Benchmark,
  BenchmarkMode,
  Level,
  Measurement,
  Mode,
  OperationsPerInvocation,
  OutputTimeUnit,
  Scope,
  Setup,
  State,
  Warmup
}

import scala.util.Random

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
@Warmup(iterations = 5, time = 200, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 10, time = 200, timeUnit = TimeUnit.MILLISECONDS)
class JavaStreamBinaryOutputBenchmark {

  val array: Array[Byte]   = Random.nextBytes(4096)
  var output: BinaryOutput = _

  @Setup(Level.Iteration)
  def setup(): Unit =
    output = new JavaStreamBinaryOutput(new ByteArrayOutputStream())

  @Benchmark
  def writeByte(): Unit =
    output.writeByte(100)

  @Benchmark
  def writeShort(): Unit =
    output.writeShort(100)

  @Benchmark
  def writeInt(): Unit =
    output.writeInt(100000)

  @Benchmark
  def writeLong(): Unit =
    output.writeLong(1000000000L)

  @Benchmark
  def writeDouble(): Unit =
    output.writeDouble(1234.1234)

  @Benchmark
  def writeBytes(): Unit =
    output.writeBytes(array)

  @Benchmark
  @OperationsPerInvocation(8)
  def writeVarInt(): Unit = {
    output.writeVarInt(1)
    output.writeVarInt(1000)
    output.writeVarInt(100000)
    output.writeVarInt(10000000)
    output.writeVarInt(-1)
    output.writeVarInt(-1000)
    output.writeVarInt(-100000)
    output.writeVarInt(-10000000)
  }
}
