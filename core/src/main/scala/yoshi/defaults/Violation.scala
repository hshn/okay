package yoshi.defaults

import scala.util.matching.Regex

enum Violation:
  case Required
  case NonIntegerString(value: String)
  case TooLongString(value: String, maxLength: Int)
  case TooShortString(value: String, minLength: Int)
  case Unmatched(value: String, pattern: Regex)
  case TooSmall(value: Int, min: Int)
  case TooLarge(value: Int, max: Int)
  case NonPositive(value: Int)
