package okay.defaults

import okay.Validation

given optionCanBeDefined[A]: Validation[Any, Violation, Option[A], A] =
  Validations.required

given stringCanBeInt: Validation[Any, Violation, String, Int] =
  Validations.parseInt
