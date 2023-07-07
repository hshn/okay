package okay

import cats.implicits._
import zio.ZIO

class Validation[-R, +V, -A, +B](
  val run: A => ZIO[R, Violations[V], B],
) { self =>

  def map[C](f: B => C): Validation[R, V, A, C] =
    Validation.instance[A] { a =>
      for {
        b <- run(a)
      } yield {
        f(b)
      }
    }

  def andValidate[C, R1 <: R, V1 >: V](f: B => ZIO[R1, Violations[V1], C]): Validation[R1, V1, A, C] =
    Validation.instance[A] { a =>
      for {
        b <- run(a)
        c <- f(b)
      } yield {
        c
      }
    }

  def |+|[R1 <: R, V1 >: V, A1 <: A, B1 >: B](next: Validation[R1, V1, A1, B1]): Validation[R1, V1, A1, B1] =
    Validation.instance { a =>
      (
        run(a),
        next.run(a),
      ).validateN {
        case (b1, b2) if b1 == b2 => b1
        case (b1, b2)             => throw new RuntimeException(s"Expected $b1 == $b2")
      }
    }

  def >>[C, R1 <: R, V1 >: V](next: Validation[R1, V1, B, C]): Validation[R1, V1, A, C] =
    andValidate(b => next.run(b))
}

object Validation extends ValidationInstances {

  def instance[A] = new InstancePartiallyApplied[A]

  final class InstancePartiallyApplied[A](private val dummy: Boolean = true) extends AnyVal {
    def apply[R, V, B](f: A => ZIO[R, Violations[V], B]): Validation[R, V, A, B] = new Validation[R, V, A, B](run = f)
  }

  def ensure[V, A](f: => V)(test: A => Boolean): Validation[Any, V, A, A] =
    ensureOr[V, A](_ => f)(test)

  def ensureOr[V, A](f: A => V)(test: A => Boolean): Validation[Any, V, A, A] = instance[A] { a =>
    if (test(a))
      ZIO.succeed(a)
    else
      ZIO.fail(Violations.single(f(a)))
  }
}

trait ValidationInstances {

  implicit def optionCanBeDefined[V: ViolationFactory, A]: Validation[Any, V, Option[A], A] =
    Validation.instance { value =>
      ZIO.fromEither(value.toRight(Violations.single[V](ViolationFactory[V].required)))
    }

  implicit def stringCanBeInt[V: ViolationFactory]: Validation[Any, V, String, Int] =
    Validation.instance { value =>
      ZIO
        .attempt(Integer.parseInt(value))
        .refineOrDie { case _: NumberFormatException =>
          Violations.single[V](ViolationFactory[V].nonInteger(value))
        }
    }

  implicit def listCanBeValidatedAs[R, V, A, B](implicit validation: Validation[R, V, A, B]): Validation[R, V, List[A], List[B]] =
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

  implicit def mapCanBeValidatedAs[R, V, A, B](implicit
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
