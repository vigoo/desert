import sbt._
import sbt.Keys._

import java.nio.charset.StandardCharsets
import java.nio.file.{FileAlreadyExistsException, Files}
import scala.meta._

object ZioSchemaGenerator extends AutoPlugin {

  case class FieldModel(field: Term.Name)
  case class ConstructorModel(name: Term.Name)

  case class CaseClassModel(arity: Int) {
    def implicitSerializerName: Term.Name   = Term.Name(s"caseClass${arity}Serializer")
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

  case class EnumModel(arity: Int) {
    def implicitSerializerName: Term.Name   = Term.Name(s"enum${arity}Serializer")
    def implicitDeserializerName: Term.Name = Term.Name(s"enum${arity}Deserializer")

    def typeParams: List[Type.Name] = (1 to arity).toList.map(n => Type.Name(s"T$n")) :+ Type.Name("Z")

    def typeParamsAsAnys: List[Type.Name] = typeParams.map(_ => t"Any")

    def typeParamDefs: List[Type.Param] = typeParams.map { name =>
      Type.Param(Nil, name, Nil, Type.Bounds(None, None), Nil, Nil)
    }

    def enumName: Type.Name = Type.Name(s"Enum$arity")

    def enumType: Type = t"Schema.$enumName[..$typeParams]"

    def enumTypeWithAnys: Type = t"Schema.$enumName[..$typeParamsAsAnys]"

    def constructors: List[ConstructorModel] =
      (1 to arity)
        .map(n => ConstructorModel(Term.Name(s"case$n")))
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

  private def generateEnumSerializer(log: Logger, targetFile: File): Unit = {
    val enums = (2 to 22).map(EnumModel).toList

    val enumSerializers: List[Stat] =
      enums.map { cc =>
        val caseRefs = cc.constructors.map(c => q"schema.${c.name}")

        q"""
         implicit def ${cc.implicitSerializerName}[..${cc.typeParamDefs}](implicit derivationContext: DerivationContext): EnumSerializer[${cc.enumType}] =
           (schema: ${cc.enumType}, value: Any) =>
             serializeCases(value)(..$caseRefs)
         """
      }

    val code =
      source"""
               package io.github.vigoo.desert.zioschema

               import io.github.vigoo.desert.AdtCodec
               import zio.schema.Schema

               import scala.collection.immutable.ListMap

               private[zioschema] trait EnumSerializer[S <: Schema.Enum[_]] {
                 def getSerializationCommands(schema: S, value: Any): List[AdtCodec.SerializationCommand]
               }

               private[zioschema] object EnumSerializer extends EnumSerializerBase {

                 ..$enumSerializers
               }
            """

    try Files.createDirectories(targetFile.toPath.getParent)
    catch {
      case _: FileAlreadyExistsException =>
    }
    Files.write(targetFile.toPath, code.toString.getBytes(StandardCharsets.UTF_8))
  }

  private def generateEnumDeserializer(log: Logger, targetFile: File): Unit = {
    val enums = (2 to 22).map(EnumModel).toList

    val enumDeserializers: List[Stat] =
      enums.map { cc =>
        val caseRefs = cc.constructors.map(c => q"schema.${c.name}")

        q"""
         implicit def ${cc.implicitDeserializerName}[..${cc.typeParamDefs}](implicit derivationContext: DerivationContext): EnumDeserializer[${cc.enumType}] =
           (schema: ${cc.enumType}) =>
             List(..$caseRefs).filterNot(isTransient).map(caseToDeserializationCommand)
         """
      }

    val code =
      source"""
               package io.github.vigoo.desert.zioschema

               import io.github.vigoo.desert.AdtCodec
               import zio.schema.Schema

               import scala.collection.immutable.ListMap

               private[zioschema] trait EnumDeserializer[S <: Schema.Enum[_]] {
                 def getDeserializationCommands(schema: S): List[AdtCodec.DeserializationCommand[EnumDeserializerBase.SchemaBuilderState]]
               }

               private[zioschema] object EnumDeserializer extends EnumDeserializerBase {

                 ..$enumDeserializers
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

      val sourcesDir         = (Compile / sourceManaged).value
      val recordSerializer   = sourcesDir / "io" / "github" / "vigoo" / "desert" / "zioschema" / "RecordSerializer.scala"
      val recordDeserializer =
        sourcesDir / "io" / "github" / "vigoo" / "desert" / "zioschema" / "RecordDeserializer.scala"
      val enumSerializer     = sourcesDir / "io" / "github" / "vigoo" / "desert" / "zioschema" / "EnumSerializer.scala"
      val enumDeserializer   = sourcesDir / "io" / "github" / "vigoo" / "desert" / "zioschema" / "EnumDeserializer.scala"

      val cachedFun = FileFunction.cached(
        streams.value.cacheDirectory / "desert-zio-schema-generators",
        FileInfo.hash
      ) { input: Set[File] =>
        generateRecordSerializer(log, recordSerializer)
        generateRecordDeserializer(log, recordDeserializer)
        generateEnumSerializer(log, enumSerializer)
        generateEnumDeserializer(log, enumDeserializer)
        Set(recordSerializer, recordDeserializer, enumSerializer, enumDeserializer)
      }

      cachedFun(Set(file("project/ZioSchemaGenerator.scala"))).toSeq
    }

  override def projectSettings: Seq[Def.Setting[_]] =
    Seq(
      Compile / sourceGenerators += generateSource.taskValue
    )
}
