package io.github.vigoo.desert

import io.github.vigoo.desert.SerializerState.{StoreStringResult, StringAlreadyStored, StringId, StringIsNew}

case class SerializerState(stringsById: Map[StringId, String],
                           idsByString: Map[String, StringId],
                           lastStringId: StringId) {

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
}
object SerializerState {
  case class StringId(value: Int) extends AnyVal {
    def next: StringId = StringId(value + 1)
  }

  val initial: SerializerState = SerializerState(
    stringsById = Map.empty,
    idsByString = Map.empty,
    lastStringId = StringId(0)
  )

  sealed trait StoreStringResult
  final case class StringAlreadyStored(id: StringId) extends StoreStringResult
  final case class StringIsNew(newId: StringId) extends StoreStringResult
}
