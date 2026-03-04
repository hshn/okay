package yoshi.defaults

import yoshi.Validation

implicit def optionCanBeDefined[A]: Validation[Any, Violation, Option[A], A] =
  Validations.required

implicit val stringCanBeInt: Validation[Any, Violation, String, Int] =
  Validations.parseInt
