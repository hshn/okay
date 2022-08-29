package okay.violation

sealed trait Violation extends Product with Serializable {}

object Violation {
  case object Required                       extends Violation
  case class NonIntegerString(value: String) extends Violation

  case class TooLongString(value: String, maxLength: Int)  extends Violation
  case class TooShortString(value: String, minLength: Int) extends Violation
}
