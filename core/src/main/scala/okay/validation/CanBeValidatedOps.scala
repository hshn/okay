package okay.validation

import okay.Validation
import okay.Violations
import zio.ZIO

class CanBeValidatedOps[A](val a: A) extends AnyVal {
  import CanBeValidatedOps._
  def okay[B] = new OkayPartiallyApplied[A, B](a)
}

object CanBeValidatedOps {
  final class OkayPartiallyApplied[A, B](val a: A) extends AnyVal {
    def apply[R, V](implicit validation: Validation[R, V, A, B]): ZIO[R, Violations[V], B] =
      validation.run(a)
  }
}
