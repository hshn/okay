package okay

import scala.util.matching.Regex
import zio.ZIO

trait Validations[V] {
  def required[A](using VF: ViolationFactory[V]): Validation[Any, V, Option[A], A] =
    Validation.instance[Option[A]] { maybeA =>
      ZIO.fromEither(maybeA.toRight(Violations.single(VF.required)))
    }

  def matches(pattern: Regex)(using VF: ViolationFactory[V]): Validation[Any, V, String, String] =
    Validation.instance[String] {
      case value @ pattern(_*) => ZIO.succeed(value)
      case other               => ZIO.fail(Violations.single(VF.unmatched(value = other, pattern = pattern)))
    }

  def maxLength(max: Int)(using VF: ViolationFactory[V]): Validation[Any, V, String, String] =
    Validation.ensureOr[V, String] { value =>
      VF.tooLongString(value, max)
    } { value =>
      value.length <= max
    }

  def minLength(min: Int)(using VF: ViolationFactory[V]): Validation[Any, V, String, String] =
    Validation.ensureOr[V, String] { value =>
      VF.tooShortString(value, min)
    } { value =>
      value.length >= min
    }

  def as[C] = new Validations.AsPartiallyApplied[V, C]
}

object Validations {
  final class AsPartiallyApplied[V, C] {
    def apply[R, B]()(using validation: Validation[R, V, B, C]): Validation[R, V, B, C] = validation
  }
}
