package io.github.vigoo.desert.docs

import io.github.vigoo.desert.DesertFailure
import mdoc._

import scala.collection.mutable
import scala.meta.inputs.Position
import scala.util.{Failure, Success, Try}

class SerializerResultModifier extends PostModifier {
  override val name: String = "serialized"

  private def renderResult(ctx: PostModifierContext, value: Any): String =
    try {
      val result = value.asInstanceOf[Either[DesertFailure, Array[Byte]]]
      result match {
        case Left(failure) =>
          s"""```
             |Left($failure)
             |```
             |""".stripMargin

        case Right(bytes) =>
          """<table style="border-collapse: initial; border: 0px"><tr>""" ++
            bytes
              .map(b =>
                s"""<td style="border: 1px solid; padding: 6px; text-align: center; background-color: rgb(154, 231, 147); margin: 0px; border-spacing: 1px; font-family: monospace; font-weight: normal">$b</td>"""
              )
              .mkString ++
            """</tr></table>"""
      }
    } catch {
      case _: Throwable =>
        val (pos, obtained) = ctx.variables.lastOption match {
          case Some(variable) =>
            val prettyObtained =
              s"${variable.staticType} = ${variable.runtimeValue}"
            (variable.pos, prettyObtained)
          case None           =>
            (Position.Range(ctx.originalCode, 0, 0), "nothing")
        }
        ctx.reporter.error(
          pos,
          s"""type mismatch:
         expected: Either[DesertFailure, Array[Byte]]
         obtained: $obtained"""
        )
        ""
    }

  override def process(ctx: PostModifierContext): String = {
    val builder = new mutable.StringBuilder()

    builder.append("```scala\n")
    builder.append(ctx.originalCode.text)
    builder.append("\n```\n\n")
    for (variable <- ctx.variables) {
      builder.append('\n')
      if (ctx.variables.size > 1) {
        builder.append(variable.name + ":\n")
      }
      builder.append(renderResult(ctx, variable.runtimeValue))
      builder.append('\n')
    }
    builder.toString()
  }
}
