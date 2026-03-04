package yoshi

import yoshi.Validation.*
import yoshi.defaults.*
import zio.Promise
import zio.ZIO
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

        for result <- validation.run("ab").either
        yield assertTrue(result.is(_.left) == expectedViolations)
      }
      test("result value when valid") {
        val validation = (Validations.minLength(1) |+| Validations.maxLength(2))
          .map(_.length)

        for result <- validation.run("ab")
        yield assertTrue(result == 2)
      }
      test("return left value when both succeed") {
        val left: Validation[Any, Violation, String, String] =
          Validation.instance[String](s => ZIO.succeed(s.toUpperCase))
        val right: Validation[Any, Violation, String, String] =
          Validation.instance[String](s => ZIO.succeed(s.toLowerCase))

        for result <- (left |+| right).run("Ab")
        yield assertTrue(result == "AB")
      }
    }
    suiteAll(">>") {
      test("chain two validations sequentially") {
        val first: Validation[Any, Violation, String, String] = Validations.minLength(1)
        val second: Validation[Any, Violation, String, Int]   = Validation.instance[String] { s =>
          ZIO.succeed(s.length)
        }

        for result <- (first >> second).run("hello")
        yield assertTrue(result == 5)
      }
      test("fail on first validation") {
        val first: Validation[Any, Violation, String, String] = Validations.minLength(10)
        val second: Validation[Any, Violation, String, Int]   = Validation.instance[String] { s =>
          ZIO.succeed(s.length)
        }

        for result <- (first >> second).run("hi").either
        yield assertTrue(result.is(_.left) == Violations.single(Violation.TooShortString("hi", 10)))
      }
      test("fail on second validation") {
        val first: Validation[Any, Violation, String, String]  = Validations.minLength(1)
        val second: Validation[Any, Violation, String, String] = Validations.maxLength(2)

        for result <- (first >> second).run("hello").either
        yield assertTrue(result.is(_.left) == Violations.single(Violation.TooLongString("hello", 2)))
      }
      test("does not run second validation when first fails") {
        for
          called <- Promise.make[Nothing, Unit]
          first  = Validations.minLength(10)
          second = Validation.instance[String](_ => called.succeed(()) *> ZIO.succeed(0))
          _         <- (first >> second).run("hi").either
          wasCalled <- called.isDone
        yield assertTrue(!wasCalled)
      }
    }
    suiteAll("orElse") {
      test("return first result when first succeeds") {
        val first: Validation[Any, Violation, String, String]  = Validations.minLength(1)
        val second: Validation[Any, Violation, String, String] = Validations.minLength(5)

        for result <- first.orElse(second).run("ab")
        yield assertTrue(result == "ab")
      }
      test("fall back to second when first fails") {
        val first: Validation[Any, Violation, String, String]  = Validations.minLength(10)
        val second: Validation[Any, Violation, String, String] = Validations.minLength(1)

        for result <- first.orElse(second).run("hello")
        yield assertTrue(result == "hello")
      }
      test("return first violation when both fail") {
        val first: Validation[Any, Violation, String, String]  = Validations.minLength(10)
        val second: Validation[Any, Violation, String, String] = Validations.maxLength(1)

        for result <- first.orElse(second).run("hello").either
        yield assertTrue(result.is(_.left) == Violations.single(Violation.TooShortString("hello", 10)))
      }
      test("does not run second when first succeeds") {
        for
          called <- Promise.make[Nothing, Unit]
          first  = Validation.instance[String](s => ZIO.succeed(s))
          second = Validation.instance[String](_ => called.succeed(()) *> ZIO.succeed("fallback"))
          result    <- first.orElse(second).run("ok")
          wasCalled <- called.isDone
        yield assertTrue(result == "ok", !wasCalled)
      }
    }
  }
}
