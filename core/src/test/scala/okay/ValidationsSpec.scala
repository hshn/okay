package okay

import okay.defaults._
import okay.defaults.Validations
import zio.Scope
import zio.test.Assertion._
import zio.test.Spec
import zio.test.TestEnvironment
import zio.test.ZIOSpecDefault
import zio.test.assertZIO

object ValidationsSpec extends ZIOSpecDefault {
  val maxLengthSuite = suite("maxLength() can test string length")(
    test("success") {
      val validation = Validations.maxLength(max = 4)
      val value      = "a".repeat(4)

      assertZIO(validation.run(value))(equalTo(value))
    },
    test("failure") {
      val validation = Validations.maxLength(max = 4)
      val value      = "a".repeat(5)

      assertZIO(validation.run(value).either)(
        isLeft(equalTo(Violations.single(Violation.TooLongString(value, maxLength = 4)))),
      )
    },
  )

  val minLengthSuite = suite("minLength() can test string length")(
    test("success") {
      val validation = Validations.minLength(min = 4)
      val value      = "a".repeat(4)

      assertZIO(validation.run(value))(equalTo(value))
    },
    test("failure") {
      val validation = Validations.minLength(min = 4)
      val value      = "a".repeat(3)

      assertZIO(validation.run(value).either)(
        isLeft(equalTo(Violations.single(Violation.TooShortString(value, minLength = 4)))),
      )
    },
  )

  val matchesSuite = suite("matches() can test with regex")(
    test("success") {
      val pattern    = "^abc$".r
      val validation = Validations.matches(pattern)
      val value      = "abc"

      assertZIO(validation.run(value).either)(
        isRight(equalTo(value)),
      )
    },
    test("failure") {
      val pattern    = "^abc$".r
      val validation = Validations.matches(pattern)
      val value      = "abcd"

      assertZIO(validation.run(value).either)(
        isLeft(equalTo(Violations.single(Violation.Unmatched(value, pattern)))),
      )
    },
  )
  override def spec: Spec[TestEnvironment with Scope, Any] = suite("Validations")(
    minLengthSuite,
    maxLengthSuite,
    matchesSuite,
  )
}
