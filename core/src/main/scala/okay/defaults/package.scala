package okay

package object defaults {
  implicit val defaultViolationFactory: ViolationFactory[Violation] = new DefaultViolationFactory {}
}
