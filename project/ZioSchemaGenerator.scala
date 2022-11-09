import sbt._
import sbt.Keys._

import java.nio.charset.StandardCharsets
import java.nio.file.{FileAlreadyExistsException, Files}
import scala.meta._

object ZioSchemaGenerator extends AutoPlugin {

  case class FieldModel(field: Term.Name)

  case class CaseClassModel(arity: Int) {
    def implicitSerializerName: Term.Name = Term.Name(s"caseClass${arity}Serializer")
    def implicitDeserializerName: Term.Name = Term.Name(s"caseClass${arity}Deserializer")

    def typeParams: List[Type.Name]       = (1 to arity).toList.map(n => Type.Name(s"T$n")) :+ Type.Name("Z")
    def typeParamsAsAnys: List[Type.Name] = typeParams.map(_ => t"Any")
    def typeParamDefs: List[Type.Param]   = typeParams.map { name =>
      Type.Param(Nil, name, Nil, Type.Bounds(None, None), Nil, Nil)
    }

    def caseClassName: Type.Name    = Type.Name(s"CaseClass$arity")
    def caseClassType: Type         = t"Schema.$caseClassName[..$typeParams]"
    def caseClassTypeWithAnys: Type = t"Schema.$caseClassName[..$typeParamsAsAnys]"

    def fields: List[FieldModel] =
      (1 to arity)
        .map(n => if (arity == 1) FieldModel(Term.Name("field")) else FieldModel(Term.Name(s"field$n")))
        .toList
  }

  private def generateRecordSerializer(log: Logger, targetFile: File): Unit = {
    val caseClasses = (1 to 22).map(CaseClassModel).toList

    val caseClassSerializers: List[Stat] =
      caseClasses.map { cc =>
        val fieldSerializerCommands =
          cc.fields.map { fm =>
            q"""AdtCodec.SerializationCommand.WriteField(
                  schema.${fm.field}.name,
                  schema.asInstanceOf[${cc.caseClassTypeWithAnys}].${fm.field}.get(value),
                  () => DerivedBinaryCodec.deriveInContext(schema.${fm.field}.schema.asInstanceOf[Schema[Any]])
                )
             """
          }

        q"""
         implicit def ${cc.implicitSerializerName}[..${cc.typeParamDefs}](implicit derivationContext: DerivationContext): RecordSerializer[${cc.caseClassType}] =
           (schema: ${cc.caseClassType}, value: Any) =>
             List(
               ..$fieldSerializerCommands
             )
         """
      }

    val code =
      source"""
               package io.github.vigoo.desert.zioschema

               import io.github.vigoo.desert.AdtCodec
               import zio.schema.Schema

               import scala.collection.immutable.ListMap

               private[zioschema] trait RecordSerializer[S <: Schema.Record[_]] {
                 def getSerializationCommands(schema: S, value: Any): List[AdtCodec.SerializationCommand]
               }

               private[zioschema] object RecordSerializer extends RecordSerializerBase {

                 ..$caseClassSerializers
               }      
            """

    try Files.createDirectories(targetFile.toPath.getParent)
    catch {
      case _: FileAlreadyExistsException =>
    }
    Files.write(targetFile.toPath, code.toString.getBytes(StandardCharsets.UTF_8))
  }

  private def generateRecordDeserializer(log: Logger, targetFile: File): Unit = {
    val caseClasses = (1 to 22).map(CaseClassModel).toList

    val caseClassDeserializers: List[Stat] =
      caseClasses.map { cc =>
        val fieldDeserializerCommands =
          cc.fields.map { fm =>
            q"""fieldToDeserializationCommand(schema.${fm.field})"""
          }

        q"""
         implicit def ${cc.implicitDeserializerName}[..${cc.typeParamDefs}](implicit derivationContext: DerivationContext): RecordDeserializer[${cc.caseClassType}] =
           (schema: ${cc.caseClassType}) =>
             List(
               ..$fieldDeserializerCommands
             )
         """
      }

    val code =
      source"""
               package io.github.vigoo.desert.zioschema

               import zio.schema.Schema
               import io.github.vigoo.desert.AdtCodec

               private[zioschema] trait RecordDeserializer[S <: Schema.Record[_]] {
                 def getDeserializationCommands(schema: S): List[AdtCodec.DeserializationCommand[RecordDeserializerBase.SchemaBuilderState]]
               }

               private[zioschema] object RecordDeserializer extends RecordDeserializerBase {

                 ..$caseClassDeserializers
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

      val sourcesDir       = (Compile / sourceManaged).value
      val recordSerializer = sourcesDir / "io" / "github" / "vigoo" / "desert" / "zioschema" / "RecordSerializer.scala"
      val recordDeserializer = sourcesDir / "io" / "github" / "vigoo" / "desert" / "zioschema" / "RecordDeserializer.scala"

      val cachedFun = FileFunction.cached(
        streams.value.cacheDirectory / "desert-zio-schema-generators",
        FileInfo.hash
      ) { input: Set[File] =>
        generateRecordSerializer(log, recordSerializer)
        generateRecordDeserializer(log, recordDeserializer)
        Set(recordSerializer, recordDeserializer)
      }

      cachedFun(Set(file("project/ZioSchemaGenerator.scala"))).toSeq
    }

  override def projectSettings: Seq[Def.Setting[_]] =
    Seq(
      Compile / sourceGenerators += generateSource.taskValue
    )
}
