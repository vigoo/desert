package io.github.vigoo.desert

sealed trait Evolution

case object InitialVersion extends Evolution

/**
 * Add a new field to a case class
 *
 * New version can still read old with the use of [[default]].
 * Old version can only read if [[F]] is Option[T] and the value is None.
 */
case class FieldAdded[F](name: String, default: F) extends Evolution

/**
 * Existing non-option field made optional
 *
 * New version can read old data by wrapping with [[Some]]
 * Old version can read new data if it is not [[None]]
 */
case class FieldMadeOptional(name: String) extends Evolution

/**
 * Field removed from product
 *
 * New version can read old data by skipping the field
 * Old version can read new data only if it was Option[T]
 */
case class FieldRemoved(name: String) extends Evolution

case class FieldKeepReferences(name: String) extends Evolution

/**
 * New constructor to a coproduct
 *
 * Old version can read if not used (old constructor IDs must remain the same)
 */
case class NewConstructor(name: String) extends Evolution