package io.github.vigoo.desert

import _root_.zio.schema.ast.SchemaAst
import _root_.zio.schema.{DynamicValue, Schema}

package object zioschema {

  implicit val schemaAstCodec: BinaryCodec[SchemaAst] =
    DerivedBinaryCodec.derive(SchemaAst.schema)

  implicit val dynamicValueCodec: BinaryCodec[DynamicValue] =
    DerivedBinaryCodec.derive(Schema.dynamicValue)

}
