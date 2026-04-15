package yoshi.defaults

import yoshi.Validation

implicit def optionCanBeDefined[A]: Validation[Violation, Option[A], A] =
  Validations.required

implicit val stringCanBeInt: Validation[Violation, String, Int] =
  Validations.parseInt
