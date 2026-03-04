package yoshi.syntax

import yoshi.Validation
import yoshi.Violations
import zio.ZIO

/** Provides `.validateAs[B]` on any value and `.at(path)` on ZIO effects that fail with [[Violations]].
  *
  * {{{
  * import yoshi.defaults.*
  *
  * val result: ZIO[Any, Violations[Violation], Int] = "42".validateAs[Int]
  * val atField: ZIO[Any, Violations[Violation], Int] = "42".validateAs[Int].at("age")
  * }}}
  */
trait ValidateAs:
  extension [A](a: A) def validateAs[B](using va: ValidatedAs[A, B]): ZIO[va.Env, Violations[va.Err], B] = va.run(a)

  extension [R, V, A](value: ZIO[R, Violations[V], A])
    def at(path: Violations.Path): ZIO[R, Violations[V], A] = value.mapError(_.asChild(path))
    def at(key: String): ZIO[R, Violations[V], A]           = at(Violations.Path.Key(key))
    def at(index: Int): ZIO[R, Violations[V], A]            = at(Violations.Path.Index(index))

/** Bridge typeclass that captures a [[Validation]] with its environment and error types as type members.
  *
  * Instances are derived automatically via `transparent inline given` from any [[Validation]] in implicit scope, allowing the compiler to
  * resolve the concrete `Env` and `Err` types at each use site.
  */
sealed trait ValidatedAs[-A, +B]:
  type Env
  type Err
  def run(a: A): ZIO[Env, Violations[Err], B]

object ValidatedAs:
  final class Derived[R, V, A, B](v: Validation[R, V, A, B]) extends ValidatedAs[A, B]:
    type Env = R
    type Err = V
    def run(a: A): ZIO[R, Violations[V], B] = v.run(a)

  transparent inline given derive[R, V, A, B](using v: Validation[R, V, A, B]): ValidatedAs[A, B] =
    new Derived(v)
