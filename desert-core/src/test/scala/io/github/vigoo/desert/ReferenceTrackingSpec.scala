package io.github.vigoo.desert

import io.github.vigoo.desert.custom._
import zio.test.Assertion._
import zio.test._

object ReferenceTrackingSpec extends ZIOSpecDefault with SerializationProperties {
  implicit val typeRegistry: TypeRegistry = TypeRegistry.empty

  final case class Root(node: Node)
  object Root {
    implicit val codec: BinaryCodec[Root] = new BinaryCodec[Root] {
      override def deserialize()(implicit ctx: DeserializationContext): Root =
        Root(readRefOrValue[Node](storeReadReference = false))

      override def serialize(value: Root)(implicit context: SerializationContext): Unit =
        storeRefOrObject(value.node)
    }
  }

  final class Node(val label: String, var next: Option[Node]) {
    override def toString: String = s"<$label>"
  }

  object Node {

    implicit lazy val codec: BinaryCodec[Node] =
      new BinaryCodec[Node] {
        override def deserialize()(implicit ctx: DeserializationContext): Node = {
          val label   = read[String]()
          val result  = new Node(label, None)
          storeReadRef(result)
          val hasNext = read[Boolean]()
          if (hasNext) {
            val next = readRefOrValue[Node](storeReadReference = false)
            result.next = Some(next)
          }
          result
        }

        override def serialize(value: Node)(implicit context: SerializationContext): Unit = {
          write(value.label)
          value.next match {
            case Some(next) =>
              write(true)
              storeRefOrObject(next)
            case None       =>
              write(false)
          }
        }
      }
  }

  override def spec: Spec[TestEnvironment, Any] =
    suite("Reference tracking")(
      test("allows the serialization of cycles") {
        val a = new Node("a", None)
        val b = new Node("b", None)
        val c = new Node("c", None)

        a.next = Some(b)
        b.next = Some(c)
        c.next = Some(a)

        val chk: Assertion[Root] = assertion("root") { root =>
          val a = root.node
          val b = a.next.get
          val c = b.next.get

          a.label == "a" && b.label == "b" && c.label == "c" && c.next.get == a
        }
        canBeSerializedAndReadBack[Root, Root](Root(a), chk)
      }
    )
}
