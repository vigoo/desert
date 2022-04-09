package io.github.vigoo.desert

import io.github.vigoo.desert.codecs._
import io.github.vigoo.desert.syntax._
import zio.test.Assertion._
import zio.test._

object ReferenceTrackingSpec extends ZIOSpecDefault with SerializationProperties {
  implicit val typeRegistry: TypeRegistry = TypeRegistry.empty

  case class Root(node: Node)
  object Root {
    implicit val codec: BinaryCodec[Root] = BinaryCodec.define[Root](
      root => storeRefOrObject(root.node)
    )(readRefOrValue[Node](storeReadReference = false).map(Root.apply))
  }

  class Node(val label: String,
             var next: Option[Node]) {
    override def toString: String = s"<$label>"
  }
  object Node {
    implicit def codec: BinaryCodec[Node] = BinaryCodec.define[Node](
      node => for {
        _ <- write(node.label)
        _ <- node.next match {
          case Some(value) =>
            for {
              _ <- write(true)
              _ <- storeRefOrObject(value)
            } yield ()
          case None =>
            write(false)
        }
      } yield ()
    )(for {
      label <- read[String]()
      result = new Node(label, None)
      _ <- storeReadRef(result)
      hasNext <- read[Boolean]()
      _ <- if (hasNext ) {
        readRefOrValue[Node](storeReadReference = false).map { value => result.next = Some(value) }
      } else finishDeserializerWith(())
    } yield result)
  }

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("Reference tracking")(
      test("allows the serialization of cycles") {
        val a = new Node("a", None)
        val b = new Node("b", None)
        val c = new Node("c", None)

        a.next = Some(b)
        b.next = Some(c)
        c.next = Some(a)

        val chk: Assertion[Root] = assertion("root")() { root =>
          val a = root.node
          val b = a.next.get
          val c = b.next.get

          a.label == "a" && b.label == "b" && c.label == "c" && c.next.get == a
        }
        canBeSerializedAndReadBack[Root, Root](Root(a), chk)
      }
    )
}
