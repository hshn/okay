package yoshi

import scala.util.matching.Regex

/** A composable validation that transforms a value of type `A` into `B`, accumulating violations of type `V` on failure.
  *
  * Validations can be composed sequentially with `>>` (short-circuit on first failure) or with `|+|` (accumulate all violations).
  *
  * {{{
  * val nonEmpty: Validation[String, String, String] =
  *   Validation.ensure("must not be empty")(_.nonEmpty)
  *
  * val maxLen: Validation[String, String, String] =
  *   Validation.ensure("too long")(_.length <= 100)
  *
  * // Accumulating: collects violations from both
  * val combined = nonEmpty |+| maxLen
  *
  * // Sequential: short-circuits on first failure
  * val chained = nonEmpty >> maxLen
  * }}}
  *
  * @tparam V
  *   violation type produced on failure
  * @tparam A
  *   input type (contravariant)
  * @tparam B
  *   output type on success (covariant)
  */
sealed abstract class Validation[+V, -A, +B] { self =>

  /** Run this validation on the given input.
    *
    * @return
    *   a `Right` with `B` on success, or a `Left` with [[Violations]] on failure
    */
  def run(a: A): Either[Violations[V], B]

  /** Transform the violation type produced on failure. */
  def mapError[V1](f: V => V1): Validation[V1, A, B] =
    Validation.instance[A] { a =>
      self.run(a).left.map(_.map(f))
    }

  /** Adapt the input type by applying `f` before validation.
    *
    * {{{
    * case class User(name: String)
    * val nonEmpty: Validation[String, String, String] = ???
    * val validateName: Validation[String, User, String] = nonEmpty.contramap(_.name)
    * }}}
    */
  def contramap[A0](f: A0 => A): Validation[V, A0, B] =
    Validation.instance[A0] { a0 =>
      self.run(f(a0))
    }

  /** Transform the output value after a successful validation. */
  def map[C](f: B => C): Validation[V, A, C] =
    Validation.instance[A] { a =>
      for {
        b <- run(a)
      } yield {
        f(b)
      }
    }

  /** Accumulating composition: run both validations on the same input and accumulate all violations.
    *
    * If both validations succeed, the result of `this` is returned.
    *
    * {{{
    * val v = nonEmpty |+| maxLength(100)
    * // If input is "" with length 0, both violations are collected
    * }}}
    */
  def |+|[V1 >: V, A1 <: A, B1 >: B](next: Validation[V1, A1, B1]): Validation[V1, A1, B1] =
    Validation.instance { a =>
      val lhs: Either[Violations[V1], B1] = run(a)
      val rhs: Either[Violations[V1], B1] = next.run(a)
      (lhs, rhs).validateN { case (b1, _) => b1 }
    }

  /** Sequential composition: run `this` first, then feed its output into `next`.
    *
    * Short-circuits on the first failure without running `next`.
    *
    * {{{
    * val parseInt: Validation[String, String, Int] = ???
    * val positive: Validation[String, Int, Int] = ???
    * val positiveInt = parseInt >> positive
    * }}}
    */
  def >>[C, V1 >: V](next: Validation[V1, B, C]): Validation[V1, A, C] =
    Validation.instance[A] { a =>
      for {
        b <- run(a)
        c <- next.run(b)
      } yield {
        c
      }
    }

  /** Fall back to `that` validation if `this` one fails.
    *
    * If `this` succeeds, its result is returned. If `this` fails, `that` is attempted; if `that` also fails, the violations from `this`
    * (the first attempt) are returned.
    */
  def orElse[V1 >: V, A1 <: A, B1 >: B](that: Validation[V1, A1, B1]): Validation[V1, A1, B1] =
    Validation.instance[A1] { a =>
      self.run(a) match {
        case Right(b)         => Right(b)
        case Left(firstError) =>
          that.run(a).left.map(_ => firstError)
      }
    }

  /** Lift this validation to handle `Option`: `None` passes through as `None`, `Some(a)` is validated and wrapped back in `Some` on
    * success.
    */
  def optional: Validation[V, Option[A], Option[B]] =
    Validation.instance[Option[A]] {
      case Some(a) => self.run(a).map(Some(_))
      case None    => Right(None)
    }
}

/** Factory methods and given instances for [[Validation]]. */
object Validation extends ValidationInstances {

  final private class Impl[+V, -A, +B](f: A => Either[Violations[V], B]) extends Validation[V, A, B] {
    def run(a: A): Either[Violations[V], B] = f(a)
  }

  /** Create a [[Validation]] from a function that returns an `Either`.
    *
    * {{{
    * val positive: Validation[String, Int, Int] =
    *   Validation.instance[Int] { n =>
    *     if (n > 0) Right(n)
    *     else Left(Violations.of("must be positive"))
    *   }
    * }}}
    */
  def instance[A] = new InstancePartiallyApplied[A]

  final class InstancePartiallyApplied[A] {
    def apply[V, B](f: A => Either[Violations[V], B]): Validation[V, A, B] = new Impl(f)
  }

  /** Create a [[Validation]] from a function that receives a [[ValidationCursor]].
    *
    * The cursor provides field access with automatic path tracking, eliminating the need for manual `.at("field")` calls. The field name is
    * extracted from the accessor lambda at compile time.
    *
    * {{{
    * val v: Validation[Violation, FormInput, Order] =
    *   Validation.cursor[FormInput] { c =>
    *     (
    *       c.validateAs[String](_.name),   // path "name" derived automatically
    *       c.validateAs[Int](_.age),        // path "age" derived automatically
    *     ).validateN { case (name, age) => Order(name, age) }
    *   }
    * }}}
    *
    * @see
    *   [[ValidationCursor]] for field access and path override
    * @see
    *   [[Validation.instance]] for the manual approach using `.at("field")`
    */
  def cursor[A] = new CursorPartiallyApplied[A]

  final class CursorPartiallyApplied[A] {
    def apply[V, B](f: ValidationCursor[A] => Either[Violations[V], B]): Validation[V, A, B] =
      new Impl(a => f(new ValidationCursor(a)))
  }

  /** A validation that always succeeds, passing the input through unchanged. */
  def succeed[A]: Validation[Nothing, A, A] =
    instance[A](a => Right(a))

  /** A validation that always fails with the given violation. */
  def fail[V](v: V): Validation[V, Any, Nothing] =
    instance[Any](_ => Left(Violations.of(v)))

  /** Validate that a predicate holds, failing with a constant violation if it does not.
    *
    * {{{
    * val nonEmpty = Validation.ensure("must not be empty")((_: String).nonEmpty)
    * }}}
    */
  def ensure[V, A](f: => V)(test: A => Boolean): Validation[V, A, A] =
    ensureOr[V, A](_ => f)(test)

  /** Validate that a predicate holds, computing the violation from the input value. */
  def ensureOr[V, A](f: A => V)(test: A => Boolean): Validation[V, A, A] = instance[A] { a =>
    if (test(a))
      Right(a)
    else
      Left(Violations.of(f(a)))
  }

  /** Extract a value from `Option`, failing with the given violation if `None`.
    *
    * {{{
    * val req = Validation.required[String, Int]("field is required")
    * req.run(Some(42)) // succeeds with 42
    * req.run(None)     // fails with "field is required"
    * }}}
    */
  def required[V, A](error: => V): Validation[V, Option[A], A] =
    instance[Option[A]] { maybeA =>
      maybeA.toRight(Violations.of(error))
    }

  /** Parse a `String` to `Int`, failing with a violation produced from the original string on `NumberFormatException`.
    */
  def parseInt[V](error: String => V): Validation[V, String, Int] =
    instance[String] { value =>
      try Right(Integer.parseInt(value))
      catch { case _: NumberFormatException => Left(Violations.of(error(value))) }
    }

  /** Validate that a string's length does not exceed `max`. */
  def maxLength[V](max: Int)(error: (String, Int) => V): Validation[V, String, String] =
    ensureOr[V, String] { value =>
      error(value, max)
    } { value =>
      value.length <= max
    }

  /** Validate that a string's length is at least `min`. */
  def minLength[V](min: Int)(error: (String, Int) => V): Validation[V, String, String] =
    ensureOr[V, String] { value =>
      error(value, min)
    } { value =>
      value.length >= min
    }

  /** Validate that an integer is greater than or equal to `n`. */
  def min[V](n: Int)(error: Int => V): Validation[V, Int, Int] =
    ensureOr[V, Int](error)(_ >= n)

  /** Validate that an integer is less than or equal to `n`. */
  def max[V](n: Int)(error: Int => V): Validation[V, Int, Int] =
    ensureOr[V, Int](error)(_ <= n)

  /** Validate that an integer is strictly positive (greater than zero). */
  def positive[V](error: Int => V): Validation[V, Int, Int] =
    ensureOr[V, Int](error)(_ > 0)

  /** Validate that a string matches the given regex pattern. */
  def matches[V](pattern: Regex)(error: (String, Regex) => V): Validation[V, String, String] =
    instance[String] { value =>
      if (pattern.matches(value)) Right(value)
      else Left(Violations.of(error(value, pattern)))
    }

}
