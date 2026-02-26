package okay.syntax

import okay.Validation
import okay.Violations
import zio.ZIO

/** Provides `.validateAs[B]` on any value and `.at(path)` on ZIO effects that fail with [[Violations]]. */
trait ValidateAsSyntax:

  /** Invoke a validation on this value, resolved from a given [[Validation]] instance.
    *
    * {{{
    * import okay.defaults.given
    *
    * val result: ZIO[Any, Violations[Violation], Int] = "42".validateAs[Int]()
    * val atField: ZIO[Any, Violations[Violation], Int] = "42".validateAs[Int].at("age")
    * }}}
    */
  extension [A](a: A) def validateAs[B]: ValidateAsPartiallyApplied[A, B] = new ValidateAsPartiallyApplied[A, B](a)

  /** Nest any violations in the error channel under the given path. */
  extension [R, V, A](value: ZIO[R, Violations[V], A])
    def at(path: Violations.Path): ZIO[R, Violations[V], A] = value.mapError(_.asChild(path))
    def at(key: String): ZIO[R, Violations[V], A]           = at(Violations.Path.Key(key))
    def at(index: Int): ZIO[R, Violations[V], A]            = at(Violations.Path.Index(index))

/** Intermediate class returned by `.validateAs[B]` that allows running the validation directly or with a path annotation via `.at(...)`.
  */
final class ValidateAsPartiallyApplied[A, B](private val a: A):

  /** Run the validation using a given [[Validation]] instance. */
  def apply[R, V](using validation: Validation[R, V, A, B]): ZIO[R, Violations[V], B] = validation.run(a)

  /** Run the validation and nest any violations under the given path. */
  def at[R, V](path: Violations.Path)(using validation: Validation[R, V, A, B]): ZIO[R, Violations[V], B] =
    validation.run(a).mapError(_.asChild(path))
  def at[R, V](key: String)(using validation: Validation[R, V, A, B]): ZIO[R, Violations[V], B] =
    at(Violations.Path.Key(key))
  def at[R, V](index: Int)(using validation: Validation[R, V, A, B]): ZIO[R, Violations[V], B] =
    at(Violations.Path.Index(index))
