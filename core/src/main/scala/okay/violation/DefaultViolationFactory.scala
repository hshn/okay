package okay.violation

import okay.ViolationFactory

trait DefaultViolationFactory extends ViolationFactory[Violation] {
  override def required: Violation                  = Violation.Required
  override def nonInteger(value: String): Violation = Violation.NonIntegerString(value)
}
