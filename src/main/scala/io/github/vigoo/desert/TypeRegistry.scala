package io.github.vigoo.desert

trait TypeRegistry {
  def get(any: Any): RegisteredType[_]
  def register[T](implicit codec: BinaryCodec[T]): Unit
}

object TypeRegistry {
  case class RegisteredTypeId(value: Int) extends AnyVal
}