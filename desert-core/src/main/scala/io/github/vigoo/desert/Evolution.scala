package io.github.vigoo.desert

import scala.annotation.StaticAnnotation

sealed trait Evolution extends Serializable

object Evolution {
  case object InitialVersion extends Evolution

  /** Add a new field to a case class
    *
    * New version can still read old with the use of [[default]]. Old version can only read if [[F]] is Option[T] and
    * the value is None.
    */
  case class FieldAdded[F](name: String, default: F) extends Evolution

  /** Existing non-option field made optional
    *
    * New version can read old data by wrapping with [[Some]] Old version can read new data if it is not [[None]]
    */
  case class FieldMadeOptional(name: String) extends Evolution

  /** Field removed from product
    *
    * New version can read old data by skipping the field Old version can read new data only if it was Option[T]
    */
  case class FieldRemoved(name: String) extends Evolution

  object FieldMadeTransient {

    /** Field made transient
      *
      * An alias for [FieldRemoved]
      *
      * New version can read old data by skipping the field Old version can read new data only if it was Option[T]
      */
    def apply(name: String): Evolution = FieldRemoved(name)
  }
}
