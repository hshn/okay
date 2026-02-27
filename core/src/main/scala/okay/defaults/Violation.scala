package okay.defaults

import scala.util.matching.Regex

/** Default violation type for use with [[Validations]].
  *
  * Each case captures the context needed to produce a meaningful error message.
  */
enum Violation:
  /** The value was `None` when a value was required. */
  case Required

  /** The string could not be parsed as an integer. */
  case NonIntegerString(value: String)

  /** The string exceeded the maximum allowed length. */
  case TooLongString(value: String, maxLength: Int)

  /** The string was shorter than the minimum required length. */
  case TooShortString(value: String, minLength: Int)

  /** The string did not match the expected regex pattern. */
  case Unmatched(value: String, pattern: Regex)

  /** The integer was below the minimum allowed value. */
  case TooSmall(value: Int, min: Int)

  /** The integer exceeded the maximum allowed value. */
  case TooLarge(value: Int, max: Int)

  /** The integer was not strictly positive. */
  case NonPositive(value: Int)
