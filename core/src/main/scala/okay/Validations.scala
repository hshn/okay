package okay

import cats.syntax.either._
import scala.util.matching.Regex

trait Validations[V] {
  def matches(pattern: Regex)(implicit VF: ViolationFactory[V]): Validation[V, String, String] =
    Validation.instance {
      case value @ pattern(_ @_*) => value.asRight
      case other                  => Violations.single(VF.unmatched(value = other, pattern = pattern)).asLeft
    }

  def maxLength(max: Int)(implicit VF: ViolationFactory[V]): Validation[V, String, String] =
    Validation.ensureOr[V, String] { value =>
      VF.tooLongString(value = value, max = max)
    } { value =>
      value.length <= max
    }

  def minLength(min: Int)(implicit VF: ViolationFactory[V]): Validation[V, String, String] =
    Validation.ensureOr[V, String] { value =>
      VF.tooShortString(value = value, min = min)
    } { value =>
      min <= value.length
    }
}
