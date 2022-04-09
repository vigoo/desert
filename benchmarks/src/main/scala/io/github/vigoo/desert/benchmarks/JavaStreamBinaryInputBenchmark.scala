package io.github.vigoo.desert.benchmarks

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.util.concurrent.TimeUnit

import io.github.vigoo.desert._
import io.github.vigoo.desert.codecs._
import io.github.vigoo.desert.syntax._
import org.openjdk.jmh.annotations._

import scala.util.Random

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
@Warmup(iterations = 10, time = 500, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 10, time = 500, timeUnit = TimeUnit.MILLISECONDS)
class JavaStreamBinaryInputBenchmark {

  val inputByte: Array[Byte] = serializeToArray(100.toByte).toOption.get
  val inputShort: Array[Byte] = serializeToArray(100.toShort).toOption.get
  val inputInt: Array[Byte] = serializeToArray(100000.toInt).toOption.get
  val inputLong: Array[Byte] = serializeToArray(1000000000L).toOption.get
  val inputFloat: Array[Byte] = serializeToArray(Math.PI.toFloat).toOption.get
  val inputDouble: Array[Byte] = serializeToArray(Math.PI).toOption.get
  val inputVarInt: Array[Byte] = {
    val stream = new ByteArrayOutputStream()
    val output = new JavaStreamBinaryOutput(stream)
    output.writeVarInt(1)
    output.writeVarInt(1000)
    output.writeVarInt(100000)
    output.writeVarInt(10000000)
    output.writeVarInt(-1)
    output.writeVarInt(-1000)
    output.writeVarInt(-100000)
    output.writeVarInt(-10000000)
    stream.close()
    stream.toByteArray
  }

  @Benchmark
  def readByte(): Unit = {
    new JavaStreamBinaryInput(new ByteArrayInputStream(inputByte)).readByte()
  }

  @Benchmark
  def readShort(): Unit = {
    new JavaStreamBinaryInput(new ByteArrayInputStream(inputShort)).readShort()
  }

  @Benchmark
  def readInt(): Unit = {
    new JavaStreamBinaryInput(new ByteArrayInputStream(inputInt)).readInt()
  }

  @Benchmark
  def readLong(): Unit = {
    new JavaStreamBinaryInput(new ByteArrayInputStream(inputLong)).readLong()
  }

  @Benchmark
  def readFloat(): Unit = {
    new JavaStreamBinaryInput(new ByteArrayInputStream(inputFloat)).readFloat()
  }

  @Benchmark
  def readDouble(): Unit = {
    new JavaStreamBinaryInput(new ByteArrayInputStream(inputDouble)).readDouble()
  }

  @Benchmark
  def readBytes(): Unit = {
    new JavaStreamBinaryInput(new ByteArrayInputStream(new Array(4096))).readBytes(4096)
  }

  @Benchmark
  @OperationsPerInvocation(8)
  def readVarInt(): Unit = {
    val input = new JavaStreamBinaryInput(new ByteArrayInputStream(inputVarInt))
    input.readVarInt(optimizeForPositive = false)
    input.readVarInt(optimizeForPositive = false)
    input.readVarInt(optimizeForPositive = false)
    input.readVarInt(optimizeForPositive = false)
    input.readVarInt(optimizeForPositive = false)
    input.readVarInt(optimizeForPositive = false)
    input.readVarInt(optimizeForPositive = false)
    input.readVarInt(optimizeForPositive = false)
  }
}
