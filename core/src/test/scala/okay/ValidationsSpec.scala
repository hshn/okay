package okay

import okay.defaults.{given, *}
import okay.defaults.Validations
import zio.test.*

object ValidationsSpec extends ZIOSpecDefault {
  override def spec = suiteAll("Validations") {
    suiteAll("required() can extract from Option") {
      test("success") {
        val validation = Validations.required[String]

        for result <- validation.run(Some("hello"))
        yield assertTrue(result == "hello")
      }
      test("failure") {
        val validation = Validations.required[String]

        for result <- validation.run(None).either
        yield assertTrue(result.is(_.left) == Violations.single(Violation.Required))
      }
    }
    suiteAll("parseInt() can parse string to int") {
      test("success") {
        val validation = Validations.parseInt

        for result <- validation.run("42")
        yield assertTrue(result == 42)
      }
      test("failure") {
        val validation = Validations.parseInt

        for result <- validation.run("abc").either
        yield assertTrue(result.is(_.left) == Violations.single(Violation.NonIntegerString("abc")))
      }
    }
    suiteAll("maxLength() can test string length") {
      test("success") {
        val validation = Validations.maxLength(max = 4)
        val value      = "a".repeat(4)

        for result <- validation.run(value)
        yield assertTrue(result == value)
      }
      test("failure") {
        val validation = Validations.maxLength(max = 4)
        val value      = "a".repeat(5)

        for result <- validation.run(value).either
        yield assertTrue(result.is(_.left) == Violations.single(Violation.TooLongString(value, maxLength = 4)))
      }
    }
    suiteAll("minLength() can test string length") {
      test("success") {
        val validation = Validations.minLength(min = 4)
        val value      = "a".repeat(4)

        for result <- validation.run(value)
        yield assertTrue(result == value)
      }
      test("failure") {
        val validation = Validations.minLength(min = 4)
        val value      = "a".repeat(3)

        for result <- validation.run(value).either
        yield assertTrue(result.is(_.left) == Violations.single(Violation.TooShortString(value, minLength = 4)))
      }
    }
    suiteAll("matches() can test with regex") {
      test("success") {
        val pattern    = "^abc$".r
        val validation = Validations.matches(pattern)
        val value      = "abc"

        for result <- validation.run(value)
        yield assertTrue(result == value)
      }
      test("failure") {
        val pattern    = "^abc$".r
        val validation = Validations.matches(pattern)
        val value      = "abcd"

        for result <- validation.run(value).either
        yield assertTrue(result.is(_.left) == Violations.single(Violation.Unmatched(value, pattern)))
      }
    }
  }
}
