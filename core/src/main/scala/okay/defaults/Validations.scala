package okay.defaults

import okay.Validation
import scala.util.matching.Regex

/** Pre-built validators using the [[Violation]] error type.
  *
  * These are convenience wrappers around [[Validation]] factory methods with [[Violation]] as the violation type, ready to use out of the
  * box.
  *
  * {{{
  * import okay.defaults.Validations
  *
  * val v = Validations.required[String]
  * v.run(Some("hello"))  // succeeds with "hello"
  * v.run(None)           // fails with Violation.Required
  * }}}
  */
object Validations {

  /** Extract a value from `Option`, failing with [[Violation.Required]] if `None`. */
  def required[A]: Validation[Any, Violation, Option[A], A] =
    Validation.required(Violation.Required)

  /** Parse a `String` to `Int`, failing with [[Violation.NonIntegerString]]. */
  def parseInt: Validation[Any, Violation, String, Int] =
    Validation.parseInt(Violation.NonIntegerString(_))

  /** Validate that a string's length does not exceed `max`, failing with [[Violation.TooLongString]].
    */
  def maxLength(max: Int): Validation[Any, Violation, String, String] =
    Validation.maxLength(max)(Violation.TooLongString(_, _))

  /** Validate that a string's length is at least `min`, failing with [[Violation.TooShortString]].
    */
  def minLength(min: Int): Validation[Any, Violation, String, String] =
    Validation.minLength(min)(Violation.TooShortString(_, _))

  /** Validate that a string matches the given regex pattern, failing with [[Violation.Unmatched]].
    */
  def matches(pattern: Regex): Validation[Any, Violation, String, String] =
    Validation.matches(pattern)(Violation.Unmatched(_, _))

  /** Look up a `Validation[Violation, B, C]` from given instances.
    *
    * {{{
    * // Resolves to the given `stringCanBeInt` instance
    * val v: Validation[Any, Violation, String, Int] = Validations.as[Int]()
    * }}}
    */
  def as[C] = new AsPartiallyApplied[C]

  final private[defaults] class AsPartiallyApplied[C] {
    def apply[R, B]()(using validation: Validation[R, Violation, B, C]): Validation[R, Violation, B, C] = validation
  }
}
