package io.github.vigoo.desert.internal

import io.github.vigoo.desert.internal.SerializerState._

import scala.collection.mutable

final class SerializerState private (
    stringsById: mutable.Map[StringId, String],
    idsByString: mutable.Map[String, StringId],
    refsById: mutable.Map[RefId, AnyRef],
    idsByRef: mutable.Map[AnyRef, RefId],
    private var lastStringId: StringId,
    private var lastRefId: RefId
) {
  def getStringById(id: StringId): Option[String] = stringsById.get(id)
  def getRefById(id: RefId): Option[AnyRef]       = refsById.get(id)

  def storeString(value: String): StoreStringResult =
    idsByString.get(value) match {
      case Some(id) => StringAlreadyStored(id)
      case None     =>
        val id = lastStringId.next
        lastStringId = id
        stringsById += (id    -> value)
        idsByString += (value -> id)
        StringIsNew(id)
    }

  def storeRef(value: AnyRef): StoreRefResult =
    idsByRef.get(value) match {
      case Some(id) => RefAlreadyStored(id)
      case None     =>
        val id = lastRefId.next
        lastRefId = id
        refsById += (id    -> value)
        idsByRef += (value -> id)
        RefIsNew(id)
    }

  def toPure: PureSerializerState =
    PureSerializerState(
      stringsById = stringsById.toMap,
      idsByString = idsByString.toMap,
      refsById = refsById.toMap,
      idsByRef = idsByRef.toMap,
      lastStringId = lastStringId,
      lastRefId = lastRefId
    )

  def resetTo(pure: PureSerializerState): Unit = {
    stringsById.clear()
    stringsById.addAll(pure.stringsById)
    idsByString.clear()
    idsByString.addAll(pure.idsByString)
    refsById.clear()
    refsById.addAll(pure.refsById)
    idsByRef.clear()
    idsByRef.addAll(pure.idsByRef)
    lastStringId = pure.lastStringId
    lastRefId = pure.lastRefId
  }
}
object SerializerState {
  final case class StringId(value: Int) extends AnyVal {
    def next: StringId = StringId(value + 1)
  }

  final case class RefId(value: Int) extends AnyVal {
    def next: RefId = RefId(value + 1)
  }

  def create: SerializerState = new SerializerState(
    stringsById = mutable.Map.empty,
    idsByString = mutable.Map.empty,
    refsById = mutable.Map.empty,
    idsByRef = mutable.Map.empty,
    lastStringId = StringId(0),
    lastRefId = RefId(0)
  )

  sealed trait StoreStringResult
  final case class StringAlreadyStored(id: StringId) extends StoreStringResult
  final case class StringIsNew(newId: StringId)      extends StoreStringResult

  sealed trait StoreRefResult
  final case class RefAlreadyStored(id: RefId) extends StoreRefResult
  final case class RefIsNew(newId: RefId)      extends StoreRefResult
}
