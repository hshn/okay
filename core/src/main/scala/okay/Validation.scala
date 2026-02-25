package okay

import cats.syntax.all.*
import scala.util.matching.Regex
import zio.ZIO

sealed abstract class Validation[-R, +V, -A, +B] { self =>

  def run(a: A): ZIO[R, Violations[V], B]

  def contramap[A0](f: A0 => A): Validation[R, V, A0, B] =
    Validation.instance[A0] { a0 =>
      self.run(f(a0))
    }

  def map[C](f: B => C): Validation[R, V, A, C] =
    Validation.instance[A] { a =>
      for {
        b <- run(a)
      } yield {
        f(b)
      }
    }

  def |+|[R1 <: R, V1 >: V, A1 <: A, B1 >: B](next: Validation[R1, V1, A1, B1]): Validation[R1, V1, A1, B1] =
    Validation.instance { a =>
      val lhs: ZIO[R1, Violations[V1], B1] = run(a)
      val rhs: ZIO[R1, Violations[V1], B1] = next.run(a)
      (lhs, rhs).validateN { case (b1, _) => b1 }
    }

  def >>[C, R1 <: R, V1 >: V](next: Validation[R1, V1, B, C]): Validation[R1, V1, A, C] =
    Validation.instance[A] { a =>
      for {
        b <- run(a)
        c <- next.run(b)
      } yield {
        c
      }
    }
}

object Validation {

  final private class Impl[-R, +V, -A, +B](f: A => ZIO[R, Violations[V], B]) extends Validation[R, V, A, B] {
    def run(a: A): ZIO[R, Violations[V], B] = f(a)
  }

  def instance[A] = new InstancePartiallyApplied[A]

  final class InstancePartiallyApplied[A](private val dummy: Boolean = true) extends AnyVal {
    def apply[R, V, B](f: A => ZIO[R, Violations[V], B]): Validation[R, V, A, B] = new Impl(f)
  }

  def succeed[A]: Validation[Any, Nothing, A, A] =
    instance[A](a => ZIO.succeed(a))

  def fail[V](v: V): Validation[Any, V, Any, Nothing] =
    instance[Any](_ => ZIO.fail(Violations.single(v)))

  def ensure[V, A](f: => V)(test: A => Boolean): Validation[Any, V, A, A] =
    ensureOr[V, A](_ => f)(test)

  def ensureOr[V, A](f: A => V)(test: A => Boolean): Validation[Any, V, A, A] = instance[A] { a =>
    if (test(a))
      ZIO.succeed(a)
    else
      ZIO.fail(Violations.single(f(a)))
  }

  def required[V, A](error: => V): Validation[Any, V, Option[A], A] =
    instance[Option[A]] { maybeA =>
      ZIO.fromEither(maybeA.toRight(Violations.single(error)))
    }

  def parseInt[V](error: String => V): Validation[Any, V, String, Int] =
    instance[String] { value =>
      ZIO
        .attempt(Integer.parseInt(value))
        .refineOrDie { case _: NumberFormatException =>
          Violations.single(error(value))
        }
    }

  def maxLength[V](max: Int)(error: (String, Int) => V): Validation[Any, V, String, String] =
    ensureOr[V, String] { value =>
      error(value, max)
    } { value =>
      value.length <= max
    }

  def minLength[V](min: Int)(error: (String, Int) => V): Validation[Any, V, String, String] =
    ensureOr[V, String] { value =>
      error(value, min)
    } { value =>
      value.length >= min
    }

  def matches[V](pattern: Regex)(error: (String, Regex) => V): Validation[Any, V, String, String] =
    instance[String] { value =>
      if (pattern.matches(value)) ZIO.succeed(value)
      else ZIO.fail(Violations.single(error(value, pattern)))
    }

  given listCanBeValidatedAs[R, V, A, B](using validation: Validation[R, V, A, B]): Validation[R, V, List[A], List[B]] =
    Validation.instance { values =>
      ZIO
        .foreach(values.zipWithIndex) { case (a, index) =>
          validation.run(a).at(index).either.map(_.toValidatedNec)
        }
        .map { results =>
          results.sequence.leftMap(_.combineAll).toEither
        }
        .absolve
    }

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
