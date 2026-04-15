package yoshi

import yoshi.defaults.*
import zio.test.*

object ValidationsSpec extends ZIOSpecDefault {
  override def spec = suiteAll("Validations") {
    suiteAll("required() can extract from Option") {
      test("success") {
        val validation = Validations.required[String]
        assertTrue(validation.run(Some("hello")) == Right("hello"))
      }
      test("failure") {
        val validation = Validations.required[String]
        assertTrue(validation.run(None) == Left(Violations.of(Violation.Required)))
      }
    }
    suiteAll("parseInt() can parse string to int") {
      test("success") {
        val validation = Validations.parseInt
        assertTrue(validation.run("42") == Right(42))
      }
      test("failure") {
        val validation = Validations.parseInt
        assertTrue(validation.run("abc") == Left(Violations.of(Violation.NonIntegerString("abc"))))
      }
    }
    suiteAll("maxLength() can test string length") {
      test("success") {
        val validation = Validations.maxLength(max = 4)
        val value      = "a".repeat(4)
        assertTrue(validation.run(value) == Right(value))
      }
      test("failure") {
        val validation = Validations.maxLength(max = 4)
        val value      = "a".repeat(5)
        assertTrue(validation.run(value) == Left(Violations.of(Violation.TooLongString(value, maxLength = 4))))
      }
    }
    suiteAll("minLength() can test string length") {
      test("success") {
        val validation = Validations.minLength(min = 4)
        val value      = "a".repeat(4)
        assertTrue(validation.run(value) == Right(value))
      }
      test("failure") {
        val validation = Validations.minLength(min = 4)
        val value      = "a".repeat(3)
        assertTrue(validation.run(value) == Left(Violations.of(Violation.TooShortString(value, minLength = 4))))
      }
    }
    suiteAll("min() can test minimum value") {
      test("success") {
        val validation = Validations.min(n = 5)
        assertTrue(validation.run(5) == Right(5))
      }
      test("failure") {
        val validation = Validations.min(n = 5)
        assertTrue(validation.run(4) == Left(Violations.of(Violation.TooSmall(value = 4, min = 5))))
      }
    }
    suiteAll("max() can test maximum value") {
      test("success") {
        val validation = Validations.max(n = 10)
        assertTrue(validation.run(10) == Right(10))
      }
      test("failure") {
        val validation = Validations.max(n = 10)
        assertTrue(validation.run(11) == Left(Violations.of(Violation.TooLarge(value = 11, max = 10))))
      }
    }
    suiteAll("positive() can test positive value") {
      test("success") {
        val validation = Validations.positive
        assertTrue(validation.run(1) == Right(1))
      }
      test("failure with zero") {
        val validation = Validations.positive
        assertTrue(validation.run(0) == Left(Violations.of(Violation.NonPositive(value = 0))))
      }
      test("failure with negative") {
        val validation = Validations.positive
        assertTrue(validation.run(-1) == Left(Violations.of(Violation.NonPositive(value = -1))))
      }
    }
    suiteAll("matches() can test with regex") {
      test("success") {
        val pattern    = "^abc$".r
        val validation = Validations.matches(pattern)
        val value      = "abc"
        assertTrue(validation.run(value) == Right(value))
      }
      test("failure") {
        val pattern    = "^abc$".r
        val validation = Validations.matches(pattern)
        val value      = "abcd"
        assertTrue(validation.run(value) == Left(Violations.of(Violation.Unmatched(value, pattern))))
      }
    }
  }
}
