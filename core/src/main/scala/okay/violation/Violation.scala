package okay.violation

sealed trait Violation extends Product with Serializable {}

object Violation {
  case object Required                       extends Violation
  case class NonIntegerString(value: String) extends Violation
}
