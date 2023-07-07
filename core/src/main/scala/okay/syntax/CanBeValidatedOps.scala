package okay.syntax

import okay.Validation
import okay.Violations
import okay.Violations.Path
import zio.ZIO

class CanBeValidatedOps[A](val a: A) extends AnyVal {
  import CanBeValidatedOps._
  def validateAs[B] = new ValidateAsPartiallyApplied[A, B](a)
}

object CanBeValidatedOps {
  final class ValidateAsPartiallyApplied[A, B](private val a: A) extends AnyVal {
    def apply[R, V](implicit validation: Validation[R, V, A, B]): ZIO[R, Violations[V], B]          = validation.run(a)
    def at[R, V](path: Path)(implicit validation: Validation[R, V, A, B]): ZIO[R, Violations[V], B] = apply.at(path)
  }
}
