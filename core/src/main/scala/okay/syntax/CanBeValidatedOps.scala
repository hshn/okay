package okay.syntax

import okay.Validation
import okay.Violations
import zio.ZIO

final class ValidateAsPartiallyApplied[A, B](private val a: A) extends AnyVal:
  def apply[R, V](using validation: Validation[R, V, A, B]): ZIO[R, Violations[V], B]                     = validation.run(a)
  def at[R, V](path: Violations.Path)(using validation: Validation[R, V, A, B]): ZIO[R, Violations[V], B] =
    validation.run(a).mapError(_.asChild(path))
