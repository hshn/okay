package okay.syntax

import okay.Violations
import scala.language.implicitConversions
import zio.ZIO

trait StandardSyntax extends TupleZValidatedSyntax {
  implicit def syntaxCanBeValidated[A](a: A): CanBeValidatedOps[A]                                = new CanBeValidatedOps[A](a)
  implicit def syntaxZValidated[R, V, A](value: ZIO[R, Violations[V], A]): ZValidatedOps[R, V, A] = new ZValidatedOps[R, V, A](value)
}
