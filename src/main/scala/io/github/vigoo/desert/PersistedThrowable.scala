package io.github.vigoo.desert

import scala.util.Try

case class PersistedThrowable(className: String,
                              message: String,
                              stackTrace: Array[StackTraceElement],
                              cause: Option[PersistedThrowable]) extends RuntimeException {
  setStackTrace(stackTrace)

  override def getMessage: String =
    s"<$className> $message"

  override def getCause: Throwable =
    cause.orNull
}

object PersistedThrowable {
  def apply(throwable: Throwable): PersistedThrowable =
    PersistedThrowable(
      className = Try(throwable.getClass).map(_.getCanonicalName).getOrElse("unknown class"),
      message = Option(throwable.getMessage).getOrElse("no message"),
      stackTrace = throwable.getStackTrace,
      cause = Option(throwable.getCause).map {
        case pe: PersistedThrowable => pe
        case throwable: Throwable => PersistedThrowable(throwable)
      }
    )
}
