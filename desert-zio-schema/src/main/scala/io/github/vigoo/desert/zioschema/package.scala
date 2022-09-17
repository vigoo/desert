package io.github.vigoo.desert

import io.github.vigoo.desert.BinaryCodec
import zio.schema.DynamicValue
import zio.schema.ast.SchemaAst

package object zioschema {

  implicit val schemaAstCodec: BinaryCodec[SchemaAst] = null // TODO

  implicit val dynamicValueCodec: BinaryCodec[DynamicValue] = null // TODO

}
