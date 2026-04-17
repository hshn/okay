package yoshi

import yoshi.Validation.*
import yoshi.defaults.*
import zio.test.*

object ValidationCombinatorsSpec extends ZIOSpecDefault {
  override def spec = suiteAll("Validation combinators") {
    suiteAll("|+|") {
      test("result violations when invalid") {
        val validation         = Validations.minLength(3) |+| Validations.maxLength(1)
        val expectedViolations = Violations(
          Vector(
            Violation.TooShortString("ab", 3),
            Violation.TooLongString("ab", 1),
          ),
        )

        assertTrue(validation.run("ab").is(_.left) == expectedViolations)
      }
      test("result value when valid") {
        val validation = (Validations.minLength(1) |+| Validations.maxLength(2))
          .map(_.length)

        for {
          result <- validation.run("ab")
        } yield {
          assertTrue(result == 2)
        }
      }
      test("return left value when both succeed") {
        val left: Validation[Violation, String, String] =
          Validation.instance[String](s => Right(s.toUpperCase))
        val right: Validation[Violation, String, String] =
          Validation.instance[String](s => Right(s.toLowerCase))

        for {
          result <- (left |+| right).run("Ab")
        } yield {
          assertTrue(result == "AB")
        }
      }
    }
    suiteAll(">>") {
      test("chain two validations sequentially") {
        val first: Validation[Violation, String, String] = Validations.minLength(1)
        val second: Validation[Violation, String, Int]   = Validation.instance[String] { s =>
          Right(s.length)
        }

        for {
          result <- (first >> second).run("hello")
        } yield {
          assertTrue(result == 5)
        }
      }
      test("fail on first validation") {
        val first: Validation[Violation, String, String] = Validations.minLength(10)
        val second: Validation[Violation, String, Int]   = Validation.instance[String] { s =>
          Right(s.length)
        }

        assertTrue((first >> second).run("hi").is(_.left) == Violations.of(Violation.TooShortString("hi", 10)))
      }
      test("fail on second validation") {
        val first: Validation[Violation, String, String]  = Validations.minLength(1)
        val second: Validation[Violation, String, String] = Validations.maxLength(2)

        assertTrue((first >> second).run("hello").is(_.left) == Violations.of(Violation.TooLongString("hello", 2)))
      }
      test("does not run second validation when first fails") {
        var called = false
        val first  = Validations.minLength(10)
        val second = Validation.instance[String] { _ => called = true; Right(0) }
        val _      = (first >> second).run("hi")
        assertTrue(!called)
      }
    }
    suiteAll("orElse") {
      test("return first result when first succeeds") {
        val first: Validation[Violation, String, String]  = Validations.minLength(1)
        val second: Validation[Violation, String, String] = Validations.minLength(5)

        for {
          result <- first.orElse(second).run("ab")
        } yield {
          assertTrue(result == "ab")
        }
      }
      test("fall back to second when first fails") {
        val first: Validation[Violation, String, String]  = Validations.minLength(10)
        val second: Validation[Violation, String, String] = Validations.minLength(1)

        for {
          result <- first.orElse(second).run("hello")
        } yield {
          assertTrue(result == "hello")
        }
      }
      test("return first violation when both fail") {
        val first: Validation[Violation, String, String]  = Validations.minLength(10)
        val second: Validation[Violation, String, String] = Validations.maxLength(1)

        assertTrue(first.orElse(second).run("hello").is(_.left) == Violations.of(Violation.TooShortString("hello", 10)))
      }
      test("does not run second when first succeeds") {
        var called = false
        val first  = Validation.instance[String](s => Right(s))
        val second = Validation.instance[String] { _ => called = true; Right("fallback") }
        val result = first.orElse(second).run("ok")
        for {
          r <- result
        } yield {
          assertTrue(r == "ok", !called)
        }
      }
    }
  }
}
