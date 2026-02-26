package okay

import cats.syntax.all.*
import scala.util.matching.Regex
import zio.ZIO

/** A composable validation that transforms a value of type `A` into `B`,
  * accumulating violations of type `V` on failure.
  *
  * Validations can be composed sequentially with `>>` (short-circuit on first failure)
  * or in parallel with `|+|` (accumulate all violations).
  *
  * {{{
  * val nonEmpty: Validation[Any, String, String, String] =
  *   Validation.ensure("must not be empty")(_.nonEmpty)
  *
  * val maxLen: Validation[Any, String, String, String] =
  *   Validation.ensure("too long")(_.length <= 100)
  *
  * // Parallel: collects violations from both
  * val combined = nonEmpty |+| maxLen
  *
  * // Sequential: short-circuits on first failure
  * val chained = nonEmpty >> maxLen
  * }}}
  *
  * @tparam R ZIO environment required to run this validation
  * @tparam V violation type produced on failure
  * @tparam A input type (contravariant)
  * @tparam B output type on success (covariant)
  */
sealed abstract class Validation[-R, +V, -A, +B] { self =>

  /** Run this validation on the given input.
    *
    * @return a ZIO effect that succeeds with `B` or fails with [[Violations]]
    */
  def run(a: A): ZIO[R, Violations[V], B]

  /** Transform the violation type produced on failure. */
  def mapError[V1](f: V => V1): Validation[R, V1, A, B] =
    Validation.instance[A] { a =>
      self.run(a).mapError(_.map(f))
    }

  /** Adapt the input type by applying `f` before validation.
    *
    * {{{
    * case class User(name: String)
    * val nonEmpty: Validation[Any, String, String, String] = ???
    * val validateName: Validation[Any, String, User, String] = nonEmpty.contramap(_.name)
    * }}}
    */
  def contramap[A0](f: A0 => A): Validation[R, V, A0, B] =
    Validation.instance[A0] { a0 =>
      self.run(f(a0))
    }

  /** Transform the output value after a successful validation. */
  def map[C](f: B => C): Validation[R, V, A, C] =
    Validation.instance[A] { a =>
      for {
        b <- run(a)
      } yield {
        f(b)
      }
    }

  /** Parallel composition: run both validations on the same input and accumulate all violations.
    *
    * If both validations succeed, the result of `this` is returned.
    *
    * {{{
    * val v = nonEmpty |+| maxLength(100)
    * // If input is "" with length 0, both violations are collected
    * }}}
    */
  def |+|[R1 <: R, V1 >: V, A1 <: A, B1 >: B](next: Validation[R1, V1, A1, B1]): Validation[R1, V1, A1, B1] =
    Validation.instance { a =>
      val lhs: ZIO[R1, Violations[V1], B1] = run(a)
      val rhs: ZIO[R1, Violations[V1], B1] = next.run(a)
      (lhs, rhs).validateN { case (b1, _) => b1 }
    }

  /** Sequential composition: run `this` first, then feed its output into `next`.
    *
    * Short-circuits on the first failure without running `next`.
    *
    * {{{
    * val parseInt: Validation[Any, String, String, Int] = ???
    * val positive: Validation[Any, String, Int, Int] = ???
    * val positiveInt = parseInt >> positive
    * }}}
    */
  def >>[C, R1 <: R, V1 >: V](next: Validation[R1, V1, B, C]): Validation[R1, V1, A, C] =
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
    * If `this` succeeds, its result is returned. If `this` fails, `that` is attempted;
    * if `that` also fails, the violations from `this` (the first attempt) are returned.
    */
  def orElse[R1 <: R, V1 >: V, A1 <: A, B1 >: B](that: Validation[R1, V1, A1, B1]): Validation[R1, V1, A1, B1] =
    Validation.instance[A1] { a =>
      self.run(a).catchAll { firstError =>
        that.run(a).mapError(_ => firstError)
      }
    }

  /** Lift this validation to handle `Option`: `None` passes through as `None`,
    * `Some(a)` is validated and wrapped back in `Some` on success.
    */
  def optional: Validation[R, V, Option[A], Option[B]] =
    Validation.instance[Option[A]] {
      case Some(a) => self.run(a).map(Some(_))
      case None    => ZIO.succeed(None)
    }
}

/** Factory methods and given instances for [[Validation]]. */
object Validation {

  final private class Impl[-R, +V, -A, +B](f: A => ZIO[R, Violations[V], B]) extends Validation[R, V, A, B] {
    def run(a: A): ZIO[R, Violations[V], B] = f(a)
  }

  /** Create a [[Validation]] from a function that returns a ZIO effect.
    *
    * {{{
    * val positive: Validation[Any, String, Int, Int] =
    *   Validation.instance[Int] { n =>
    *     if (n > 0) ZIO.succeed(n)
    *     else ZIO.fail(Violations.single("must be positive"))
    *   }
    * }}}
    */
  def instance[A] = new InstancePartiallyApplied[A]

  final class InstancePartiallyApplied[A] {
    def apply[R, V, B](f: A => ZIO[R, Violations[V], B]): Validation[R, V, A, B] = new Impl(f)
  }

  /** A validation that always succeeds, passing the input through unchanged. */
  def succeed[A]: Validation[Any, Nothing, A, A] =
    instance[A](a => ZIO.succeed(a))

  /** A validation that always fails with the given violation. */
  def fail[V](v: V): Validation[Any, V, Any, Nothing] =
    instance[Any](_ => ZIO.fail(Violations.single(v)))

  /** Validate that a predicate holds, failing with a constant violation if it does not.
    *
    * {{{
    * val nonEmpty = Validation.ensure("must not be empty")((_: String).nonEmpty)
    * }}}
    */
  def ensure[V, A](f: => V)(test: A => Boolean): Validation[Any, V, A, A] =
    ensureOr[V, A](_ => f)(test)

  /** Validate that a predicate holds, computing the violation from the input value. */
  def ensureOr[V, A](f: A => V)(test: A => Boolean): Validation[Any, V, A, A] = instance[A] { a =>
    if (test(a))
      ZIO.succeed(a)
    else
      ZIO.fail(Violations.single(f(a)))
  }

  /** Extract a value from `Option`, failing with the given violation if `None`.
    *
    * {{{
    * val req = Validation.required[String, Int]("field is required")
    * req.run(Some(42)) // succeeds with 42
    * req.run(None)     // fails with "field is required"
    * }}}
    */
  def required[V, A](error: => V): Validation[Any, V, Option[A], A] =
    instance[Option[A]] { maybeA =>
      ZIO.fromEither(maybeA.toRight(Violations.single(error)))
    }

  /** Parse a `String` to `Int`, failing with a violation produced from the original string
    * on `NumberFormatException`.
    */
  def parseInt[V](error: String => V): Validation[Any, V, String, Int] =
    instance[String] { value =>
      ZIO
        .attempt(Integer.parseInt(value))
        .refineOrDie { case _: NumberFormatException =>
          Violations.single(error(value))
        }
    }

  /** Validate that a string's length does not exceed `max`. */
  def maxLength[V](max: Int)(error: (String, Int) => V): Validation[Any, V, String, String] =
    ensureOr[V, String] { value =>
      error(value, max)
    } { value =>
      value.length <= max
    }

  /** Validate that a string's length is at least `min`. */
  def minLength[V](min: Int)(error: (String, Int) => V): Validation[Any, V, String, String] =
    ensureOr[V, String] { value =>
      error(value, min)
    } { value =>
      value.length >= min
    }

  /** Validate that a string matches the given regex pattern. */
  def matches[V](pattern: Regex)(error: (String, Regex) => V): Validation[Any, V, String, String] =
    instance[String] { value =>
      if (pattern.matches(value)) ZIO.succeed(value)
      else ZIO.fail(Violations.single(error(value, pattern)))
    }

  /** Automatically lifts a `Validation[R, V, A, B]` to `Validation[R, V, Option[A], Option[B]]`.
    * `None` passes through; `Some(a)` is validated.
    */
  given optionCanBeValidatedAs[R, V, A, B](using validation: Validation[R, V, A, B]): Validation[R, V, Option[A], Option[B]] =
    validation.optional

  /** Automatically validates each element of a `Seq`, accumulating violations by index. */
  given seqCanBeValidatedAs[R, V, A, B](using validation: Validation[R, V, A, B]): Validation[R, V, Seq[A], Seq[B]] =
    Validation.instance { values =>
      ZIO
        .foreach(values.toList.zipWithIndex) { case (a, index) =>
          validation.run(a).at(index).either.map(_.toValidatedNec)
        }
        .map { results =>
          results.sequence.leftMap(_.combineAll).toEither
        }
        .absolve
    }

  /** Automatically validates each element of a `List`, accumulating violations by index. */
  given listCanBeValidatedAs[R, V, A, B](using Validation[R, V, A, B]): Validation[R, V, List[A], List[B]] =
    seqCanBeValidatedAs[R, V, A, B].contramap[List[A]](identity).map(_.toList)

  /** Automatically validates each value of a `Map[String, A]`, accumulating violations by key. */
  given mapCanBeValidatedAs[R, V, A, B](using
    validation: Validation[R, V, A, B],
  ): Validation[R, V, Map[String, A], Map[String, B]] =
    Validation.instance { values =>
      ZIO
        .foreach(values.toList) { case (key, a) =>
          validation.run(a).at(key).map(b => key -> b).either.map(_.toValidatedNec)
        }
        .map { results =>
          results.sequence
            .map(_.toMap)
            .leftMap(_.combineAll)
            .toEither
        }
        .absolve
    }
}
