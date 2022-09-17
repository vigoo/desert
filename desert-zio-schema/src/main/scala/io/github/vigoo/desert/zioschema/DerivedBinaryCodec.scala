package io.github.vigoo.desert.zioschema

import io.github.vigoo.desert.{
  AdtCodec,
  BinaryCodec,
  DeserializationFailure,
  Evolution,
  InitialVersion,
  SerializationFailure,
  codecs,
  evolutionSteps
}
import zio.Chunk
import zio.schema.Schema

object DerivedBinaryCodec {

  def derive[T](implicit schema: Schema[T]): BinaryCodec[T] =
    schema match {
      case enum: Schema.Enum[_]                          =>
        deriveEnum(getEvolutionStepsFromAnnotation(enum.annotations), enum)
      case record: Schema.Record[_]                      =>
        deriveRecord(getEvolutionStepsFromAnnotation(record.annotations), record)
      case transform: Schema.Transform[_, _, _]          =>
        val binaryCodec = derive(transform.codec).asInstanceOf[BinaryCodec[Any]]
        BinaryCodec.from(
          binaryCodec.contramapOrFail(
            transform.f.asInstanceOf[Any => Either[String, Any]](_).left.map(SerializationFailure(_, None))
          ),
          binaryCodec.mapOrFail(
            transform.g.asInstanceOf[Any => Either[String, T]](_).left.map(DeserializationFailure(_, None))
          )
        )
      case Schema.Lazy(inner)                            =>
        derive(inner())
      case map: Schema.MapSchema[_, _]                   =>
        codecs.mapCodec(derive(map.ks), derive(map.vs)).asInstanceOf[BinaryCodec[T]]
      case set: Schema.SetSchema[_]                      =>
        codecs.setCodec(derive(set.as)).asInstanceOf[BinaryCodec[T]]
      case sequence: Schema.Sequence[_, _, _]            => ???
      case Schema.Primitive(standardType, annotations)   => ???
      case Schema.Optional(codec, annotations)           => ???
      case Schema.Fail(message, annotations)             => ???
      case Schema.Tuple(left, right, annotations)        => ???
      case Schema.EitherSchema(left, right, annotations) => ???
      case Schema.Meta(ast, annotations)                 => ???
      case Schema.Dynamic(annotations)                   => ???
      case Schema.SemiDynamic(defaultValue, annotations) => ???
    }

  private def deriveRecord[T](evolutionSteps: Vector[Evolution], schema: Schema.Record[T]): BinaryCodec[T] = {
    println(s"deriveRecord with $evolutionSteps")
    new AdtCodec[T, SchemaBuilderState](
      evolutionSteps = ???,
      typeName = schema.id.name,
      constructors = ???,
      transientFields = ???,
      getSerializationCommands = ???,
      deserializationCommands = ???,
      initialBuilderState = ???,
      materialize = ???
    )
  }

  private def deriveEnum[T](evolutionSteps: Vector[Evolution], schema: Schema.Enum[T]): BinaryCodec[T] =
    new AdtCodec[T, SchemaBuilderState](
      evolutionSteps = ???,
      typeName = schema.id.name,
      constructors = ???,
      transientFields = ???,
      getSerializationCommands = ???,
      deserializationCommands = ???,
      initialBuilderState = ???,
      materialize = ???
    )

  private def getEvolutionStepsFromAnnotation(value: Chunk[Any]): Vector[Evolution] =
    value.collectFirst { case ann: evolutionSteps => ann } match {
      case None             => Vector(InitialVersion)
      case Some(annotation) => InitialVersion +: annotation.steps.toVector
    }

  private case class SchemaBuilderState()
}
