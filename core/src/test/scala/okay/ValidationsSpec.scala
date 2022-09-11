package okay

import okay.defaults._
import okay.defaults.Validations
import zio.Scope
import zio.test.Assertion._
import zio.test.Spec
import zio.test.TestEnvironment
import zio.test.ZIOSpecDefault
import zio.test.assertZIO

class ValidationsSpec extends ZIOSpecDefault {
  val maxLengthSuite = suite("maxLength() can test string length")(
    test("success") {
      val validation = Validations.maxLength(max = 4)

      assertZIO(validation.run("a".repeat(4)))(equalTo("a".repeat(4)))
    },
    test("failure") {
      val validation = Validations.maxLength(max = 4)

      assertZIO(validation.run("a".repeat(5)).either)(
        isLeft(equalTo(Violations.single(Violation.TooLongString("a".repeat(5), maxLength = 4)))),
      )
    },
  )

  val minLengthSuite = suite("minLength() can test string length")(
    test("success") {
      val validation = Validations.minLength(min = 4)

      assertZIO(validation.run("a".repeat(4)))(equalTo("a".repeat(4)))
    },
    test("failure") {
      val validation = Validations.minLength(min = 4)
      assertZIO(validation.run("a".repeat(3)).either)(
        isLeft(equalTo(Violations.single(Violation.TooLongString("a".repeat(3), maxLength = 4)))),
      )
    },
  )
  val matchesSuite = suite("matches() can test with regex")(
    test("success") {
      val pattern    = "^abc$".r
      val validation = Validations.matches(pattern)

      assertZIO(validation.run("abcd"))(
        equalTo("abcd"),
      )
    },
    test("failure") {
      val pattern    = "^abc$".r
      val validation = Validations.matches(pattern)

      assertZIO(validation.run("abc"))(equalTo("abc"))
    },
  )
  override def spec: Spec[TestEnvironment with Scope, Any] = suite("Validations")(
    minLengthSuite,
    maxLengthSuite,
    matchesSuite,
  )
}
