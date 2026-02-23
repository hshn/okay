package okay

package object defaults {
  given defaultViolationFactory: ViolationFactory[Violation] = new DefaultViolationFactory {}
}
