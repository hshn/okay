package okay.defaults

import okay.*
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

  def min(n: Int): Validation[Any, Violation, Int, Int] =
    Validation.min(n)(Violation.TooSmall(_, n))

  def max(n: Int): Validation[Any, Violation, Int, Int] =
    Validation.max(n)(Violation.TooLarge(_, n))

  def positive: Validation[Any, Violation, Int, Int] =
    Validation.positive(Violation.NonPositive(_))

}
