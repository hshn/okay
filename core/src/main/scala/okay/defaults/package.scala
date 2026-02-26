package okay.defaults

import okay.Validation

/** Given instance enabling `Option[A].validateAs[A]` with [[Violation.Required]]. */
given optionCanBeDefined[A]: Validation[Any, Violation, Option[A], A] =
  Validations.required

/** Given instance enabling `String.validateAs[Int]` with [[Violation.NonIntegerString]]. */
given stringCanBeInt: Validation[Any, Violation, String, Int] =
  Validations.parseInt
