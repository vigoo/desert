package io.github.vigoo.desert.zioschema

import io.github.vigoo.desert.BinaryCodec
import zio.schema.Schema

import java.util.concurrent.{ConcurrentHashMap, ConcurrentMap}

// TODO: possible need to store promises to allow caching recursive types?
private[zioschema] class DerivationContext {
  private val cache: ConcurrentMap[Schema[_], BinaryCodec[_]] = new ConcurrentHashMap()

  def get[T](schema: Schema[T]): Option[BinaryCodec[T]] =
    Option(cache.get(schema)).map(_.asInstanceOf[BinaryCodec[T]])

  def put[T](schema: Schema[T], codec: BinaryCodec[T]): BinaryCodec[T] = {
    val result = cache.putIfAbsent(schema, codec).asInstanceOf[BinaryCodec[T]]
    if (result == null)
      codec
    else
      result
  }
}
