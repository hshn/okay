package okay

import cats.Traverse
import cats.implicits._

case class Validation[+V, A, B](
  validate: A => Either[Violations[V], B],
) { self =>
  def at(path: Violations.Path): Validation[V, A, B]  = mapViolations(_.asChild(path))
  def as[C]: ValidationAsPartiallyApplied[V, A, B, C] = new ValidationAsPartiallyApplied[V, A, B, C](this)

  def map[B1](f: B => B1): Validation[V, A, B1] =
    Validation.instance { a =>
      validate(a).map(f)
    }

  def mapViolations[V1 >: V](f: Violations[V] => Violations[V1]): Validation[V1, A, B] =
    Validation.instance[A] { a =>
      validate(a).leftMap(f)
    }

  def andValidate[C, V1 >: V](f: B => Either[Violations[V1], C]): Validation[V1, A, C] =
    Validation.instance[A] { a =>
      for {
        b <- validate(a)
        c <- f(b)
      } yield {
        c
      }
    }

  def |+|[V1 >: V](other: Validation[V1, A, B]): Validation[V1, A, B] = {
    Validation.instance { a =>
      (
        validate(a).toValidatedNec,
        other.validate(a).toValidatedNec,
      ).mapN {
        case (b1, b2) if b1 == b2 => b1
        case (b1, b2)             => throw new IllegalStateException(s"$b1, $b2")
      }.leftMap(_.combineAll).toEither
    }
  }
}

object Validation extends ValidationInstances {
  def forProduct[A]: ForProductPartiallyApplied[A] = new ForProductPartiallyApplied[A]()

  final class ForProductPartiallyApplied[A](private val dummy: Boolean = true) extends AnyVal {
    def apply[V, A1, B](
      v1: Validation[Nothing, A, A] => Validation[V, A, A1],
    )(g: A1 => B): Validation[V, A, B] = {
      val product = okay[A]

      Validation.instance[A] { value =>
        v1(product)
          .validate(value)
          .map(g)
      }
    }
    def apply[V, A1, A2, B](
      v1: Validation[Nothing, A, A] => Validation[V, A, A1],
      v2: Validation[Nothing, A, A] => Validation[V, A, A2],
    )(g: (A1, A2) => B): Validation[V, A, B] = {
      val product = okay[A]

      Validation.instance[A] { value =>
        (
          v1(product).validate(value).toValidatedNec,
          v2(product).validate(value).toValidatedNec,
        ).mapN(g).leftMap(_.combineAll).toEither
      }
    }
    def apply[V, A1, A2, A3, B](
      v1: Validation[Nothing, A, A] => Validation[V, A, A1],
      v2: Validation[Nothing, A, A] => Validation[V, A, A2],
      v3: Validation[Nothing, A, A] => Validation[V, A, A3],
    )(g: (A1, A2, A3) => B): Validation[V, A, B] = {
      val product = okay[A]

      Validation.instance[A] { value =>
        (
          v1(product).validate(value).toValidatedNec,
          v2(product).validate(value).toValidatedNec,
          v3(product).validate(value).toValidatedNec,
        ).mapN(g).leftMap(_.combineAll).toEither
      }
    }
    def apply[V, A1, A2, A3, A4, B](
      v1: Validation[Nothing, A, A] => Validation[V, A, A1],
      v2: Validation[Nothing, A, A] => Validation[V, A, A2],
      v3: Validation[Nothing, A, A] => Validation[V, A, A3],
      v4: Validation[Nothing, A, A] => Validation[V, A, A4],
    )(g: (A1, A2, A3, A4) => B): Validation[V, A, B] = {
      val product = okay[A]

      Validation.instance[A] { value =>
        (
          v1(product).validate(value).toValidatedNec,
          v2(product).validate(value).toValidatedNec,
          v3(product).validate(value).toValidatedNec,
          v4(product).validate(value).toValidatedNec,
        ).mapN(g).leftMap(_.combineAll).toEither
      }
    }
  }

  def instance[A]: InstancePartiallyApplied[A] = new InstancePartiallyApplied[A]()

  private val Okay: Validation[Nothing, Any, Any] = instance(_.asRight)
  def okay[A]: Validation[Nothing, A, A]          = Okay.asInstanceOf[Validation[Nothing, A, A]]

  final class InstancePartiallyApplied[A](private val dummy: Boolean = true) extends AnyVal {
    def apply[V, B](f: A => Either[Violations[V], B]): Validation[V, A, B] = new Validation[V, A, B](f)
  }

  def ensure[V, A](f: => V)(test: A => Boolean): Validation[V, A, A] =
    ensureOr[V, A](_ => f)(test)

  def ensureOr[V, A](f: A => V)(test: A => Boolean): Validation[V, A, A] = instance { a =>
    if (test(a))
      a.asRight
    else
      Violations.single(f(a)).asLeft
  }
}

final class ValidationAsPartiallyApplied[+V, A, B, C](val value: Validation[V, A, B]) extends AnyVal {
  def apply[V1 >: V]()(implicit validation: Validation[V1, B, C]): Validation[V1, A, C] =
    value.andValidate(validation.validate)
}

trait ValidationInstances {
  implicit def requiredValidation[V: ViolationFactory, A]: Validation[V, Option[A], A] = Validation.instance { value =>
    value.toRight(Violations.single[V](ViolationFactory[V].required))
  }

  implicit def integerStringValidation[V: ViolationFactory]: Validation[V, String, Int] = Validation.instance { value =>
    try {
      Integer.parseInt(value).asRight
    } catch {
      case _: NumberFormatException => Violations.single[V](ViolationFactory[V].nonInteger(value)).asLeft
    }
  }

  implicit def traverseValidation[G[_]: Traverse, V, A, B](implicit validation: Validation[V, A, B]): Validation[V, G[A], G[B]] =
    Validation.instance { values =>
      values.zipWithIndex
        .map { case (a, index) =>
          validation.at(index).validate(a).toValidatedNec
        }
        .sequence
        .leftMap(_.combineAll).toEither
    }

  implicit def stringMapValidation[V, A, B](implicit validation: Validation[V, A, B]): Validation[V, Map[String, A], Map[String, B]] =
    Validation.instance { values =>
      values
        .map { case (key, a) =>
          validation
            .at(key).validate(a)
            .map(b => key -> b)
            .toValidatedNec
        }
        .toList
        .sequence
        .map(_.toMap)
        .leftMap(_.combineAll).toEither
    }
}
