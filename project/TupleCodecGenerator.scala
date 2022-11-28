import sbt._
import sbt.Keys._

import java.nio.charset.StandardCharsets
import java.nio.file.{FileAlreadyExistsException, Files}
import scala.meta._

object TupleCodecGenerator extends AutoPlugin {
  case class TupleModel(size: Int) {
    val typeParamTypes: List[Type.Name]             = (1 to size).map(i => Type.Name(s"T$i")).toList
    val typeParams: List[Type.Param]                =
      typeParamTypes.map(name => Type.Param(Nil, name, Nil, Type.Bounds(None, None), Nil, Nil))
    val typeParamsWithConstraints: List[Type.Param] =
      typeParamTypes.map(name =>
        Type.Param(
          Nil,
          name,
          Nil,
          Type.Bounds(None, None),
          Nil,
          List(
            Type.Name("BinaryCodec"),
            Type.Name("TupleFieldReader")
          )
        )
      )

    val tupleName: String = s"Tuple${size}"

    val builderName: String    = s"Tuple${size}Builder"
    val builderType: Type.Name = Type.Name(builderName)

    val codecName: Term.Name = Term.Name(s"tuple${size}Codec")

    val tupleLit: Lit.String     = Lit.String(tupleName)
    val tupleFullLit: Lit.String = Lit.String("scala." + tupleName)
    val appliedTupleType: Type   = t"${Type.Name(tupleName)}[..$typeParamTypes]"
    val appliedBuilderType: Type = t"$builderType[..$typeParamTypes]"
    val appliedBuilderInit       = Init(appliedBuilderType, Name.Anonymous(), Nil)

    val fromFields                = Term.Apply(
      Term.ApplyType(Term.Name(tupleName), typeParamTypes),
      (1 to size).map(i => Term.Name(s"_$i")).toList
    )
    val fieldVars: List[Defn.Var] = typeParamTypes.zipWithIndex.map { case (t, i) =>
      Defn.Var(Nil, List(Pat.Var(Term.Name(s"_${i + 1}"))), Some(t), None)
    }
  }

  private def generateCode(log: Logger, targetFile: File): Unit = {
    log.info(s"Generating tuple codecs to $targetFile")
    Files.createDirectories(targetFile.getParentFile.toPath)

    val maxSize = 22
    val tuples  = (1 to maxSize).map(TupleModel).toList

    val tupleBuilders = tuples.map { model =>
      q"""final class ${model.builderType}[..${model.typeParams}] {
            def asTuple: Either[DesertFailure, ${model.appliedTupleType}] = Right(${model.fromFields})

            ..${model.fieldVars}
          }
      """
    }

    val tupleCodecs = tuples.map { model =>
      val serializationCommands =
        model.typeParamTypes.zipWithIndex.map { case (t, i) =>
          q"""AdtCodec.SerializationCommand.WriteField(${Lit.String(s"_${i + 1}")}, tuple.${Term.Name(
              s"_${i + 1}"
            )}, () => implicitly[BinaryCodec[$t]])"""
        }

      val deserializationCommands =
        model.typeParamTypes.zipWithIndex.map { case (t, i) =>
          q"""implicitly[TupleFieldReader[$t]]
               .asCommand(
                 ${Lit.String(s"_${i + 1}")},
                 (value: $t, builder: ${model.appliedBuilderType}) => {
                   builder.${Term.Name(s"_${i + 1}")} = value; builder
                 }
               )"""
        }

      q"""implicit def ${model.codecName}[..${model.typeParamsWithConstraints}]: BinaryCodec[${model.appliedTupleType}] =
            new AdtCodec[${model.appliedTupleType}, ${model.appliedBuilderType}](
              evolutionSteps = Vector(Evolution.InitialVersion),
              typeName = ${model.tupleFullLit},
              constructors = Vector(${model.tupleLit}),
              transientFields = Map.empty,
              getSerializationCommands = (tuple: ${model.appliedTupleType}) =>
                List(
                  ..$serializationCommands
                ),
              deserializationCommands = List(
                ..$deserializationCommands
              ),
              () => new ${model.appliedBuilderInit},
              _.asTuple
            )
       """
    }

    val code =
      source"""
          package io.github.vigoo.desert.internal

          import io.github.vigoo.desert._

          trait TupleCodecs {
            def optionCodec[T: BinaryCodec]: BinaryCodec[Option[T]]

            trait TupleFieldReader[T] {
              def asCommand[TupleBuilder](
                  name: String,
                  store: (T, TupleBuilder) => TupleBuilder
              ): AdtCodec.DeserializationCommand[TupleBuilder]
            }

            object TupleFieldReader extends LowerPriorityTupleFieldReader {
              implicit def optionalFieldReader[T: BinaryCodec]: TupleFieldReader[Option[T]] =
                new TupleFieldReader[Option[T]] {
                  override def asCommand[TupleBuilder](
                      name: String,
                      store: (Option[T], TupleBuilder) => TupleBuilder
                  ): AdtCodec.DeserializationCommand[TupleBuilder] =
                    AdtCodec.DeserializationCommand.ReadOptional(
                      name,
                      () => implicitly[BinaryCodec[T]],
                      () => optionCodec(implicitly[BinaryCodec[T]]),
                      store
                    )
                }
            }

            trait LowerPriorityTupleFieldReader {
              implicit def requiredFieldReader[T: BinaryCodec]: TupleFieldReader[T] =
                new TupleFieldReader[T] {
                  override def asCommand[TupleBuilder](
                      name: String,
                      store: (T, TupleBuilder) => TupleBuilder
                  ): AdtCodec.DeserializationCommand[TupleBuilder] =
                    AdtCodec.DeserializationCommand.Read(
                      name,
                      () => implicitly[BinaryCodec[T]],
                      store
                    )
                }
            }

            ..$tupleBuilders
            ..$tupleCodecs
          }
          """

    try Files.createDirectories(targetFile.toPath.getParent)
    catch {
      case _: FileAlreadyExistsException =>
    }
    Files.write(targetFile.toPath, code.toString.getBytes(StandardCharsets.UTF_8))
  }

  lazy val generateSource =
    Def.task {
      val log = streams.value.log

      val sourcesDir = (Compile / sourceManaged).value
      val targetFile = sourcesDir / "io" / "github" / "vigoo" / "desert" / "internal" / "TupleCodecs.scala"

      val cachedFun = FileFunction.cached(
        streams.value.cacheDirectory / "desert-tuple-codecs",
        FileInfo.hash
      ) { input: Set[File] =>
        generateCode(log, targetFile)
        Set(targetFile)
      }

      cachedFun(Set(file("project/TupleCodecGenerator.scala"))).toSeq
    }

  override def projectSettings: Seq[Def.Setting[_]] =
    Seq(
      Compile / sourceGenerators += generateSource.taskValue
    )
}
