package okay

import scala.util.matching.Regex
import zio.ZIO

trait Validations[V] {
  def required[A](implicit VF: ViolationFactory[V]): Validation[Any, V, Option[A], A] =
    Validation.instance[Option[A]] { maybeA =>
      ZIO.fromEither(maybeA.toRight(Violations.single(VF.required)))
    }

  def matches(pattern: Regex)(implicit VF: ViolationFactory[V]): Validation[Any, V, String, String] =
    Validation.instance[String] {
      case value @ pattern(_ @_*) => ZIO.succeed(value)
      case other                  => ZIO.fail(Violations.single(VF.unmatched(value = other, pattern = pattern)))
    }

  def maxLength(max: Int)(implicit VF: ViolationFactory[V]): Validation[Any, V, String, String] =
    Validation.ensureOr[V, String] { value =>
      VF.tooLongString(value, max)
    } { value =>
      value.length <= max
    }

  def minLength(min: Int)(implicit VF: ViolationFactory[V]): Validation[Any, V, String, String] =
    Validation.ensureOr[V, String] { value =>
      VF.tooShortString(value, min)
    } { value =>
      value.length >= min
    }

  def as[C] = new Validations.AsPartiallyApplied[V, C]
}

object Validations {
  final class AsPartiallyApplied[V, C] {
    def apply[R, B]()(implicit validation: Validation[R, V, B, C]): Validation[R, V, B, C] = validation
  }
}
