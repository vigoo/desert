package io.github.vigoo.desert

import io.github.vigoo.desert.TypeRegistry.RegisteredTypeId

trait TypeRegistry {
  def get(any: Any): Option[RegisteredType[_]]
  def forId(id: RegisteredTypeId): Option[RegisteredType[_]]
}

object TypeRegistry {
  case class RegisteredTypeId(value: Int) extends AnyVal {
    def next: RegisteredTypeId = RegisteredTypeId(value + 1)
  }

  val empty: TypeRegistry = new TypeRegistry {
    override def get(any: Any): Option[RegisteredType[_]] = None
    override def forId(id: RegisteredTypeId): Option[RegisteredType[_]] = None
  }
}