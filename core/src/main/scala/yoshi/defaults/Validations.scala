package yoshi.defaults

import scala.util.matching.Regex
import yoshi.*

object Validations {

  def required[A]: Validation[Violation, Option[A], A] =
    Validation.required(Violation.Required)

  def parseInt: Validation[Violation, String, Int] =
    Validation.parseInt(Violation.NonIntegerString(_))

  def maxLength(max: Int): Validation[Violation, String, String] =
    Validation.maxLength(max)(Violation.TooLongString(_, _))

  def minLength(min: Int): Validation[Violation, String, String] =
    Validation.minLength(min)(Violation.TooShortString(_, _))

  def matches(pattern: Regex): Validation[Violation, String, String] =
    Validation.matches(pattern)(Violation.Unmatched(_, _))

  def min(n: Int): Validation[Violation, Int, Int] =
    Validation.min(n)(Violation.TooSmall(_, n))

  def max(n: Int): Validation[Violation, Int, Int] =
    Validation.max(n)(Violation.TooLarge(_, n))

  def positive: Validation[Violation, Int, Int] =
    Validation.positive(Violation.NonPositive(_))

}
