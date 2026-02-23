package okay.syntax

import okay.Violations
import zio.ZIO

trait StandardSyntax extends TupleZValidatedSyntax:
  extension [A](a: A) def validateAs[B]: ValidateAsPartiallyApplied[A, B] = new ValidateAsPartiallyApplied[A, B](a)
  extension [R, V, A](value: ZIO[R, Violations[V], A])
    def at(path: Violations.Path): ZIO[R, Violations[V], A] = value.mapError(_.asChild(path))
