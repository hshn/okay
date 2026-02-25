package okay

package object defaults {
  given optionCanBeDefined[A]: Validation[Any, Violation, Option[A], A] =
    Validations.required

  given stringCanBeInt: Validation[Any, Violation, String, Int] =
    Validations.parseInt
}
