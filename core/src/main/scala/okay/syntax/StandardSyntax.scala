package okay.syntax

import okay.Violations
import zio.ZIO

import scala.language.implicitConversions

trait StandardSyntax extends TupleZValidatedSyntax {
  implicit def syntaxCanBeValidated[A](a: A): CanBeValidatedOps[A]                                = new CanBeValidatedOps[A](a)
  implicit def syntaxZValidated[R, V, A](value: ZIO[R, Violations[V], A]): ZValidatedOps[R, V, A] = new ZValidatedOps[R, V, A](value)
}
