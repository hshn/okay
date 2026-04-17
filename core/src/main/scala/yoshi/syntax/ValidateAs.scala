package yoshi.syntax

import yoshi.Validation
import yoshi.Violations

/** Provides `.validateAs[B]` on any value and `.at(path)` on `Either` values that fail with [[Violations]].
  *
  * {{{
  * import yoshi.defaults.*
  *
  * val result: Either[Violations[Violation], Int] = "42".validateAs[Int]
  * val atField: Either[Violations[Violation], Int] = "42".validateAs[Int].at("age")
  * }}}
  */
trait ValidateAs:
  extension [A](a: A) def validateAs[B](using va: ValidatedAs[A, B]): Either[Violations[va.Err], B] = va.run(a)

  extension [V, A](value: Either[Violations[V], A])
    def at(path: Violations.Path): Either[Violations[V], A] = value.left.map(_.asChild(path))
    def at(key: String): Either[Violations[V], A]           = at(Violations.Path.Key(key))
    def at(index: Int): Either[Violations[V], A]            = at(Violations.Path.Index(index))

/** Bridge typeclass that captures a [[Validation]] with its error type as a type member.
  *
  * Instances are derived automatically via `transparent inline given` from any [[Validation]] in implicit scope, allowing the compiler to
  * resolve the concrete `Err` type at each use site.
  */
sealed trait ValidatedAs[-A, +B]:
  type Err
  def run(a: A): Either[Violations[Err], B]

object ValidatedAs:
  final class Derived[V, A, B](v: Validation[V, A, B]) extends ValidatedAs[A, B]:
    type Err = V
    def run(a: A): Either[Violations[V], B] = v.run(a)

  transparent inline given derive[V, A, B](using v: Validation[V, A, B]): ValidatedAs[A, B] =
    new Derived(v)
