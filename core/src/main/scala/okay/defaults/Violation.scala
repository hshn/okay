package okay.defaults

import scala.util.matching.Regex

enum Violation:
  case Required
  case NonIntegerString(value: String)
  case TooLongString(value: String, maxLength: Int)
  case TooShortString(value: String, minLength: Int)
  case Unmatched(value: String, pattern: Regex)
