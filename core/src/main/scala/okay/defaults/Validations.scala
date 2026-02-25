package okay.defaults

import okay.Validation
import scala.util.matching.Regex

object Validations {
  def required[A]: Validation[Any, Violation, Option[A], A] =
    Validation.required(Violation.Required)

  def parseInt: Validation[Any, Violation, String, Int] =
    Validation.parseInt(Violation.NonIntegerString(_))

  def maxLength(max: Int): Validation[Any, Violation, String, String] =
    Validation.maxLength(max)(Violation.TooLongString(_, _))

  def minLength(min: Int): Validation[Any, Violation, String, String] =
    Validation.minLength(min)(Violation.TooShortString(_, _))

  def matches(pattern: Regex): Validation[Any, Violation, String, String] =
    Validation.matches(pattern)(Violation.Unmatched(_, _))

  def as[C] = new AsPartiallyApplied[C]

  final private[defaults] class AsPartiallyApplied[C] {
    def apply[R, B]()(using validation: Validation[R, Violation, B, C]): Validation[R, Violation, B, C] = validation
  }
}
