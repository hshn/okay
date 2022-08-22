package okay.violation

object implicits {
  implicit val violationFactory = new DefaultViolationFactory {}
}
