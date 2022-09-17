package io.github.vigoo.desert

import _root_.zio.schema.ast.SchemaAst
import _root_.zio.schema.DynamicValue

package object zioschema {

  implicit val schemaAstCodec: BinaryCodec[SchemaAst] = null // TODO

  implicit val dynamicValueCodec: BinaryCodec[DynamicValue] = null // TODO

}
