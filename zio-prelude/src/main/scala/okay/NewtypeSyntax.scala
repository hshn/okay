package okay

import zio.ZIO
import zio.prelude.Newtype

extension (self: Validation.type)

  /** Validate and wrap a value into a [[zio.prelude.Newtype]], running its runtime assertion via `make`.
    *
    * The `error` function receives the rejected value and the assertion failure message produced by `zio.prelude` (e.g.
    * `"0 did not satisfy greaterThan(0)"`). The message format is an internal detail of zio-prelude's `AssertionError`.
    *
    * {{{
    * object PositiveInt extends Newtype[Int] {
    *   override inline def assertion = Assertion.greaterThan(0)
    * }
    *
    * val v = Validation.newtype(PositiveInt)((value, msg) => s"invalid: $value ($msg)")
    * }}}
    */
  def newtype[V, A](nt: Newtype[A])(error: (A, String) => V): Validation[Any, V, A, nt.Type] =
    Validation.instance[A] { a =>
      nt.make(a).toEither match {
        case Right(value) => ZIO.succeed(value)
        case Left(errors) => ZIO.fail(Violations(errors.map(error(a, _)).toVector))
      }
    }
