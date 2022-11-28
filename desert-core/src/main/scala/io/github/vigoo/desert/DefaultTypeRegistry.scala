package io.github.vigoo.desert

import io.github.vigoo.desert.TypeRegistry.RegisteredTypeId

import scala.reflect.ClassTag

class DefaultTypeRegistry(byId: Map[RegisteredTypeId, RegisteredType[_]], ordered: List[RegisteredType[_]])
    extends TypeRegistry {

  override def get(any: Any): Option[RegisteredType[_]] =
    ordered.find(reg => reg.cls.isAssignableFrom(any.getClass))

  override def forId(id: RegisteredTypeId): Option[RegisteredType[_]] =
    byId.get(id)
}

object DefaultTypeRegistry {
  final case class DefaultTypeRegistryBuilder(
      byId: Map[RegisteredTypeId, RegisteredType[_]],
      ordered: List[RegisteredType[_]],
      lastId: RegisteredTypeId
  ) {
    def register[T](implicit codec: BinaryCodec[T], tag: ClassTag[T]): DefaultTypeRegistryBuilder = {
      val id  = lastId.next
      val reg = RegisteredType(id, codec, tag.runtimeClass)
      copy(
        byId = byId + (id -> reg),
        ordered = reg :: ordered,
        lastId = id
      )
    }

    def registerPlaceholder(): DefaultTypeRegistryBuilder =
      copy(
        lastId = lastId.next
      )

    def freeze(): TypeRegistry = new DefaultTypeRegistry(byId, ordered)
  }

  def apply(): DefaultTypeRegistryBuilder = DefaultTypeRegistryBuilder(Map.empty, List.empty, RegisteredTypeId(0))
}
