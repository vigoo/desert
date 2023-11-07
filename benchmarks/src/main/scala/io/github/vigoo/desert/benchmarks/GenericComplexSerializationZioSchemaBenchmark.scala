package io.github.vigoo.desert.benchmarks

import io.github.vigoo.desert.Evolution.FieldAdded
import io.github.vigoo.desert.zioschema._
import io.github.vigoo.desert._
import org.openjdk.jmh.annotations._
import zio.schema._

import java.util.concurrent.TimeUnit
import scala.util.Random

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
class GenericComplexSerializationZioSchemaBenchmark {
  import GenericComplexSerializationZioSchemaBenchmark._

  var testDocument: Root          = _
  var serializedData: Array[Byte] = _

  @Param(Array("12"))
  var size: Int = _

  @Param(Array("34253"))
  var seed: Long = _

  private def generateItem(s: Int)(implicit rnd: Random): Item =
    rnd.nextInt(3) match {
      case 0           => Text(rnd.nextString(25))
      case 1           =>
        Rectangle(
          rnd.nextDouble(),
          rnd.nextDouble(),
          rnd.nextDouble(),
          rnd.nextDouble(),
          Style(
            Color(rnd.nextInt().toByte, rnd.nextInt().toByte, rnd.nextInt().toByte),
            Color(rnd.nextInt().toByte, rnd.nextInt().toByte, rnd.nextInt().toByte)
          )
        )
      case 2 if s == 0 => Text(rnd.nextString(25))
      case _           => Block(generateItemMap(s - 1))
    }

  private def generateItemMap(s: Int)(implicit rnd: Random): Map[ItemId, Item] =
    if (s == 0) {
      Map.empty
    } else {
      val count = rnd.nextInt(s)
      (0 to count).map(_ => (ItemId(rnd.nextString(16)) -> generateItem(s))).toMap
    }

  @Setup
  def createTestDocument(): Unit = {
    implicit val random: Random = new Random(seed)

    println("Generating test document")
    testDocument = Root(
      title = "A random generated test document",
      items = generateItemMap(size)
    )

    println("Serializing test document")
    serializedData = serializeToArray(testDocument).toOption.get
//    println(s"Test document: $testDocument")
//    println(s"Serialized length: ${serializedData.length}")
  }

  @Benchmark
  def serializeComplexDataStructure(): Either[DesertFailure, Array[Byte]] =
    serializeToArray(testDocument)

  @Benchmark
  def deserializeComplexDataStructure(): Either[DesertFailure, Root] =
    deserializeFromArray[Root](serializedData)
}

object Profile extends App {
  val benchmark = new GenericComplexSerializationZioSchemaBenchmark
  benchmark.size = 12
  benchmark.seed = 34253
  benchmark.createTestDocument()
  while (true)
    println(benchmark.deserializeComplexDataStructure().toOption.get.title)
}

object GenericComplexSerializationZioSchemaBenchmark {

  case class ItemId(value: String) extends AnyVal
  object ItemId {
    implicit val schema: Schema[ItemId]     = DeriveSchema.gen[ItemId]
    implicit val codec: BinaryCodec[ItemId] = DerivedBinaryCodec.deriveForWrapper[ItemId]
  }

  case class Color(r: Byte, g: Byte, b: Byte)
  object Color {
    implicit val schema: Schema[Color]     = DeriveSchema.gen[Color]
    implicit val codec: BinaryCodec[Color] = DerivedBinaryCodec.derive
  }

  case class Style(background: Color, foreground: Color)
  object Style {
    implicit val schema: Schema[Style]     = DeriveSchema.gen[Style]
    implicit val codec: BinaryCodec[Style] = DerivedBinaryCodec.derive
  }

  sealed trait Item
  case class Text(value: String)                                                 extends Item
  @evolutionSteps(FieldAdded("style", Style(Color(0, 0, 0), Color(255.toByte, 255.toByte, 255.toByte))))
  case class Rectangle(x: Double, y: Double, w: Double, h: Double, style: Style) extends Item
  case class Block(items: Map[ItemId, Item])                                     extends Item

  object Item {
    implicit val schema: Schema[Item]         = DeriveSchema.gen[Item]
    implicit val itemCodec: BinaryCodec[Item] = DerivedBinaryCodec.derive
  }

  case class Root(title: String, items: Map[ItemId, Item])
  object Root {
    implicit val schema: Schema[Root]     = DeriveSchema.gen[Root]
    implicit val codec: BinaryCodec[Root] = DerivedBinaryCodec.derive
  }
}
