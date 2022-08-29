package okay.defaults

import okay.ViolationFactory
import scala.util.matching.Regex

trait DefaultViolationFactory extends ViolationFactory[Violation] {
  override def required: Violation                                      = Violation.Required
  override def nonInteger(value: String): Violation                     = Violation.NonIntegerString(value)
  override def tooShortString(value: String, minLength: Int): Violation = Violation.TooShortString(value = value, minLength = minLength)
  override def tooLongString(value: String, maxLength: Int): Violation  = Violation.TooLongString(value = value, maxLength = maxLength)
  override def unmatched(value: String, pattern: Regex): Violation      = Violation.Unmatched(value = value, pattern = pattern)
}
