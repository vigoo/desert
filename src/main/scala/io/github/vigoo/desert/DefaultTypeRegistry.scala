package io.github.vigoo.desert

class DefaultTypeRegistry extends TypeRegistry {
  override def get(any: Any): RegisteredType[_] = ???

  override def register[T](implicit codec: BinaryCodec[T]): Unit = ???
}
