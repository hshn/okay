package okay.syntax

import okay.Validation
import okay.Violations
import zio.ZIO

trait StandardSyntax extends TupleZValidatedSyntax:
  extension [A](a: A) def validateAs[B]: ValidateAsPartiallyApplied[A, B] = new ValidateAsPartiallyApplied[A, B](a)
  extension [R, V, A](value: ZIO[R, Violations[V], A])
    def at(path: Violations.Path): ZIO[R, Violations[V], A] = value.mapError(_.asChild(path))
    def at(key: String): ZIO[R, Violations[V], A]           = at(Violations.Path.Key(key))
    def at(index: Int): ZIO[R, Violations[V], A]            = at(Violations.Path.Index(index))

final class ValidateAsPartiallyApplied[A, B](private val a: A):
  def apply[R, V](using validation: Validation[R, V, A, B]): ZIO[R, Violations[V], B]                     = validation.run(a)
  def at[R, V](path: Violations.Path)(using validation: Validation[R, V, A, B]): ZIO[R, Violations[V], B] =
    validation.run(a).mapError(_.asChild(path))
  def at[R, V](key: String)(using validation: Validation[R, V, A, B]): ZIO[R, Violations[V], B] =
    at(Violations.Path.Key(key))
  def at[R, V](index: Int)(using validation: Validation[R, V, A, B]): ZIO[R, Violations[V], B] =
    at(Violations.Path.Index(index))
