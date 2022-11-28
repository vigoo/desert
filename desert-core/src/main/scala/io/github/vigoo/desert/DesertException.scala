package io.github.vigoo.desert

/** Exception form of [[DesertFailure]] to be used in places where failure must be encoded by an exception (Future, Cats
  * Effect IO, etc).
  *
  * @param failure
  *   The failure to represent in the exception
  */
final case class DesertException(failure: DesertFailure) extends Exception(failure.message) {
  failure.cause.foreach(initCause)
}
