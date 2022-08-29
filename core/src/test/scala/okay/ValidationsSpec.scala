package okay

import cats.syntax.either._
import munit.FunSuite
import okay.defaults._
import okay.defaults.Validations._

class ValidationsSpec extends FunSuite {
  test("maxLength() can test string length") {
    val validation = maxLength(max = 4)

    assertEquals(
      validation.validate("a".repeat(5)),
      Violations.single(Violation.TooLongString("a".repeat(5), maxLength = 4)).asLeft,
    )

    assertEquals(
      validation.validate("a".repeat(4)),
      "a".repeat(4).asRight,
    )
  }
  test("minLength() can test string length") {
    val validation = minLength(min = 4)

    assertEquals(
      validation.validate("a".repeat(3)),
      Violations.single(Violation.TooShortString("a".repeat(3), minLength = 4)).asLeft,
    )

    assertEquals(
      validation.validate("a".repeat(4)),
      "a".repeat(4).asRight,
    )
  }
  test("matches() can test with regex") {
    val pattern    = "^abc$".r
    val validation = matches(pattern)

    assertEquals(
      validation.validate("abcd"),
      Violations.single(Violation.Unmatched("abcd", pattern)).asLeft,
    )

    assertEquals(
      validation.validate("abc"),
      "abc".asRight,
    )
  }
}
