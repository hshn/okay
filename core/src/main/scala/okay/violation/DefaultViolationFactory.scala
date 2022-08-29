package okay.violation

import okay.ViolationFactory

trait DefaultViolationFactory extends ViolationFactory[Violation] {
  override def required: Violation                                      = Violation.Required
  override def nonInteger(value: String): Violation                     = Violation.NonIntegerString(value)
  override def tooShortString(value: String, minLength: Int): Violation = Violation.TooShortString(value = value, minLength = minLength)
  override def tooLongString(value: String, maxLength: Int): Violation  = Violation.TooLongString(value = value, maxLength = maxLength)
}
