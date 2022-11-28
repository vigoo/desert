package io.github.vigoo.desert.internal

import io.github.vigoo.desert.internal.SerializerState._

case class PureSerializerState(
    stringsById: Map[StringId, String],
    idsByString: Map[String, StringId],
    refsById: Map[RefId, AnyRef],
    idsByRef: Map[AnyRef, RefId],
    lastStringId: StringId,
    lastRefId: RefId
) {

  def storeString(value: String): (PureSerializerState, StoreStringResult) =
    idsByString.get(value) match {
      case Some(id) => (this, StringAlreadyStored(id))
      case None     =>
        val id = lastStringId.next
        (
          copy(
            stringsById = stringsById + (id    -> value),
            idsByString = idsByString + (value -> id),
            lastStringId = id
          ),
          StringIsNew(id)
        )
    }

  def storeRef(value: AnyRef): (PureSerializerState, StoreRefResult) =
    idsByRef.get(value) match {
      case Some(id) => (this, RefAlreadyStored(id))
      case None     =>
        val id = lastRefId.next
        (
          copy(
            refsById = refsById + (id    -> value),
            idsByRef = idsByRef + (value -> id),
            lastRefId = id
          ),
          RefIsNew(id)
        )
    }
}
