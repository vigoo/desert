package io.github.vigoo.desert

import io.github.vigoo.desert.SerializerState.{RefAlreadyStored, RefId, RefIsNew, StoreRefResult, StoreStringResult, StringAlreadyStored, StringId, StringIsNew}

case class SerializerState(stringsById: Map[StringId, String],
                           idsByString: Map[String, StringId],
                           refsById: Map[RefId, AnyRef],
                           idsByRef: Map[AnyRef, RefId],
                           lastStringId: StringId,
                           lastRefId: RefId) {

  def storeString(value: String): (SerializerState, StoreStringResult) = {
    idsByString.get(value) match {
      case Some(id) => (this, StringAlreadyStored(id))
      case None =>
        val id = lastStringId.next
        (copy(
          stringsById = stringsById + (id -> value),
          idsByString = idsByString + (value -> id),
          lastStringId = id
        ), StringIsNew(id))
    }
  }

  def storeRef(value: AnyRef): (SerializerState, StoreRefResult) = {
    idsByRef.get(value) match {
      case Some(id) => (this, RefAlreadyStored(id))
      case None =>
        val id = lastRefId.next
        (copy(
          refsById = refsById + (id -> value),
          idsByRef = idsByRef + (value -> id),
          lastRefId = id
        ), RefIsNew(id))
    }
  }
}
object SerializerState {
  case class StringId(value: Int) extends AnyVal {
    def next: StringId = StringId(value + 1)
  }

  case class RefId(value: Int) extends AnyVal {
    def next: RefId = RefId(value + 1)
  }

  val initial: SerializerState = SerializerState(
    stringsById = Map.empty,
    idsByString = Map.empty,
    refsById = Map.empty,
    idsByRef = Map.empty,
    lastStringId = StringId(0),
    lastRefId = RefId(0)
  )

  sealed trait StoreStringResult
  final case class StringAlreadyStored(id: StringId) extends StoreStringResult
  final case class StringIsNew(newId: StringId) extends StoreStringResult

  sealed trait StoreRefResult
  final case class RefAlreadyStored(id: RefId) extends StoreRefResult
  final case class RefIsNew(newId: RefId) extends StoreRefResult
}
