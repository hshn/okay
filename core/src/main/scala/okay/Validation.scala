package okay

import cats.implicits._
import okay.Violations.Path
import zio.ZIO

class Validation[-R, +V, -A, +B](
  val run: A => ZIO[R, Violations[V], B],
) { self =>

  def as[C]: Validation.AsPartiallyApplied[R, V, A, B, C] =
    new Validation.AsPartiallyApplied[R, V, A, B, C](self)

  def at(path: Path): Validation[R, V, A, B] =
    Validation.instance[A] { a =>
      run(a).mapError(_.asChild(path))
    }

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
      (for {
        left  <- run(a).either.map(_.toValidatedNec)
        right <- next.run(a).either.map(_.toValidatedNec)
      } yield {
        (left, right)
          .mapN {
            case (b1, b2) if b1 == b2 => b1
            case (b1, b2)             => throw new IllegalStateException(s"Expected $b1 == $b2")
          }
          .leftMap(_.combineAll).toEither
      }).absolve
    }

  def >>[C, R1 <: R, V1 >: V](next: Validation[R1, V1, B, C]): Validation[R1, V1, A, C] =
    andValidate(b => next.run(b))
}

object Validation extends ZValidationInstances {

  def instance[A] = new InstancePartiallyApplied[A]

  final class InstancePartiallyApplied[A]() {
    def apply[R, V, B](f: A => ZIO[R, Violations[V], B]) =
      new Validation[R, V, A, B](run = f)
  }

  def ref[A]: Validation[Any, Nothing, A, A] = instance[A](ZIO.succeed(_))

  def forProduct[A]: ForProductPartiallyApplied[A] = new ForProductPartiallyApplied[A]
  final class ForProductPartiallyApplied[A]() {
    def apply[R, V, B1, C](
      v1: Validation[Any, Nothing, A, A] => Validation[R, V, A, B1],
    )(g: B1 => C): Validation[R, V, A, C] =
      instance[A] { a =>
        val root = ref[A]

        (for {
          b1 <- v1(root).run(a).either.map(_.toValidatedNec)
        } yield {
          b1.map(g).leftMap(_.combineAll).toEither
        }).absolve
      }
    def apply[R, V, B1, B2, C](
      v1: Validation[Any, Nothing, A, A] => Validation[R, V, A, B1],
      v2: Validation[Any, Nothing, A, A] => Validation[R, V, A, B2],
    )(g: (B1, B2) => C): Validation[R, V, A, C] =
      instance[A] { a =>
        val root = ref[A]

        (for {
          b1 <- v1(root).run(a).either.map(_.toValidatedNec)
          b2 <- v2(root).run(a).either.map(_.toValidatedNec)
        } yield (
          b1,
          b2,
        ).mapN(g).leftMap(_.combineAll).toEither).absolve
      }
    def apply[R, V, B1, B2, B3, C](
      v1: Validation[Any, Nothing, A, A] => Validation[R, V, A, B1],
      v2: Validation[Any, Nothing, A, A] => Validation[R, V, A, B2],
      v3: Validation[Any, Nothing, A, A] => Validation[R, V, A, B3],
    )(g: (B1, B2, B3) => C): Validation[R, V, A, C] =
      instance[A] { a =>
        val root = ref[A]

        (for {
          b1 <- v1(root).run(a).either.map(_.toValidatedNec)
          b2 <- v2(root).run(a).either.map(_.toValidatedNec)
          b3 <- v3(root).run(a).either.map(_.toValidatedNec)
        } yield (
          b1,
          b2,
          b3,
        ).mapN(g).leftMap(_.combineAll).toEither).absolve
      }
    def apply[R, V, B1, B2, B3, B4, C](
      v1: Validation[Any, Nothing, A, A] => Validation[R, V, A, B1],
      v2: Validation[Any, Nothing, A, A] => Validation[R, V, A, B2],
      v3: Validation[Any, Nothing, A, A] => Validation[R, V, A, B3],
      v4: Validation[Any, Nothing, A, A] => Validation[R, V, A, B4],
    )(g: (B1, B2, B3, B4) => C): Validation[R, V, A, C] =
      instance[A] { a =>
        val root = ref[A]

        (for {
          b1 <- v1(root).run(a).either.map(_.toValidatedNec)
          b2 <- v2(root).run(a).either.map(_.toValidatedNec)
          b3 <- v3(root).run(a).either.map(_.toValidatedNec)
          b4 <- v4(root).run(a).either.map(_.toValidatedNec)
        } yield (
          b1,
          b2,
          b3,
          b4,
        ).mapN(g).leftMap(_.combineAll).toEither).absolve
      }
  }

  final class AsPartiallyApplied[-R, +V, -A, +B, C](val self: Validation[R, V, A, B]) extends AnyVal {
    def apply[R1 <: R, V1 >: V, B1 >: B](implicit validation: Validation[R1, V1, B1, C]): Validation[R1, V1, A, C] =
      self.andValidate(validation.run)
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

trait ZValidationInstances {

  implicit def requiredValidation[V: ViolationFactory, A]: Validation[Any, V, Option[A], A] =
    Validation.instance { value =>
      ZIO.fromEither(value.toRight(Violations.single[V](ViolationFactory[V].required)))
    }

  implicit def integerStringValidation[V: ViolationFactory]: Validation[Any, V, String, Int] =
    Validation.instance { value =>
      ZIO
        .attempt(Integer.parseInt(value))
        .refineOrDie { case _: NumberFormatException =>
          Violations.single[V](ViolationFactory[V].nonInteger(value))
        }
    }

  implicit def listValidation[R, V, A, B](implicit validation: Validation[R, V, A, B]): Validation[R, V, List[A], List[B]] =
    Validation.instance { values =>
      ZIO
        .foreach(values.zipWithIndex) { case (a, index) =>
          validation.at(index).run(a).either.map(_.toValidatedNec)
        }
        .map { results =>
          results.sequence.leftMap(_.combineAll).toEither
        }
        .absolve
    }

  implicit def stringMapValidation[R, V, A, B](implicit
    validation: Validation[R, V, A, B],
  ): Validation[R, V, Map[String, A], Map[String, B]] =
    Validation.instance { values =>
      ZIO
        .foreach(values.toList) { case (key, a) =>
          validation.at(key).run(a).map(b => key -> b).either.map(_.toValidatedNec)
        }
        .map { results =>
          results.sequence
            .map(_.toMap)
            .leftMap(_.combineAll).toEither
        }
        .absolve
    }
}
