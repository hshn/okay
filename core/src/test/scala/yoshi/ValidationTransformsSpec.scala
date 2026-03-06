package yoshi

import yoshi.defaults.*
import zio.Promise
import zio.ZIO
import zio.test.*

object ValidationTransformsSpec extends ZIOSpecDefault {
  override def spec = suiteAll("Validation transforms") {
    suiteAll("succeed") {
      test("returns the input unchanged") {
        for result <- Validation.succeed[String].run("hello")
        yield assertTrue(result == "hello")
      }
      test("is right identity for >>") {
        val v = Validations.minLength(1)
        for result <- (v >> Validation.succeed).run("ab")
        yield assertTrue(result == "ab")
      }
      test("is left identity for >>") {
        val v = Validations.minLength(1)
        for result <- (Validation.succeed >> v).run("ab")
        yield assertTrue(result == "ab")
      }
    }
    suiteAll("fail") {
      test("always fails with the given violation") {
        val v = Validation.fail[Violation](Violation.Required)
        for result <- v.run("anything").either
        yield assertTrue(result.is(_.left) == Violations.of[Violation](Violation.Required))
      }
    }
    suiteAll("contramap") {
      test("adapt input type before validation") {
        val nonEmpty: Validation[Any, Violation, String, String]          = Validations.minLength(1)
        val nameValidation: Validation[Any, Violation, NameInput, String] =
          nonEmpty.contramap(_.name.getOrElse(""))

        for result <- nameValidation.run(NameInput(name = Some("Alice")))
        yield assertTrue(result == "Alice")
      }
      test("propagate violations from adapted input") {
        val minLength3: Validation[Any, Violation, String, String]        = Validations.minLength(3)
        val nameValidation: Validation[Any, Violation, NameInput, String] =
          minLength3.contramap(_.name.getOrElse(""))

        for result <- nameValidation.run(NameInput(name = Some("ab"))).either
        yield assertTrue(result.is(_.left) == Violations(Vector(Violation.TooShortString("ab", 3))))
      }
    }
    suiteAll("mapError") {
      test("transform error type on failure") {
        val v = Validations.minLength(3).mapError(_.toString)

        for result <- v.run("ab").either
        yield assertTrue(result.is(_.left) == Violations.of(Violation.TooShortString("ab", 3).toString))
      }
      test("preserve success value") {
        val v = Validations.minLength(1).mapError(_.toString)

        for result <- v.run("hello")
        yield assertTrue(result == "hello")
      }
      test("transform nested violations preserving structure") {
        val v = Validation
          .instance[String] { s =>
            ZIO.fail(
              Violations[Violation](
                values = Vector(Violation.Required),
                children = Map(
                  Violations.Path("child") -> Violations.of(Violation.TooShortString(s, 3)),
                ),
              ),
            )
          }
          .mapError(_.toString)

        for result <- v.run("ab").either
        yield assertTrue(
          result.is(_.left) == Violations[String](
            values = Vector(Violation.Required.toString),
            children = Map(
              Violations.Path("child") -> Violations.of(Violation.TooShortString("ab", 3).toString),
            ),
          ),
        )
      }
      test("compose with >>") {
        val first: Validation[Any, Violation, String, String]  = Validations.minLength(1)
        val second: Validation[Any, Violation, String, String] = Validations.maxLength(3)
        val v                                                  = (first >> second).mapError(_.toString)

        for result <- v.run("hello").either
        yield assertTrue(result.is(_.left) == Violations.of(Violation.TooLongString("hello", 3).toString))
      }
      test("compose with |+|") {
        val v = (Validations.minLength(3) |+| Validations.maxLength(1)).mapError(_.toString)

        for result <- v.run("ab").either
        yield assertTrue(
          result.is(_.left) == Violations(
            Vector(
              Violation.TooShortString("ab", 3).toString,
              Violation.TooLongString("ab", 1).toString,
            ),
          ),
        )
      }
    }
    suiteAll("optional") {
      test("pass through None as success") {
        val v = Validations.minLength(3).optional
        for result <- v.run(None)
        yield assertTrue(result == None)
      }
      test("validate Some value and wrap result in Some") {
        val v = Validations.minLength(3).optional
        for result <- v.run(Some("hello"))
        yield assertTrue(result == Some("hello"))
      }
      test("fail when Some value is invalid") {
        val v = Validations.minLength(3).optional
        for result <- v.run(Some("ab")).either
        yield assertTrue(result.is(_.left) == Violations.of(Violation.TooShortString("ab", 3)))
      }
      test("compose with map") {
        val v = Validations.minLength(1).optional.map(_.map(_.length))
        for result <- v.run(Some("hello"))
        yield assertTrue(result == Some(5))
      }
      test("compose with sequential >>") {
        val v = (Validations.minLength(1) >> Validations.maxLength(5)).optional
        for result <- v.run(Some("abc"))
        yield assertTrue(result == Some("abc"))
      }
      test("fail in sequential >> composition") {
        val v = (Validations.minLength(1) >> Validations.maxLength(3)).optional
        for result <- v.run(Some("hello")).either
        yield assertTrue(result.is(_.left) == Violations.of(Violation.TooLongString("hello", 3)))
      }
      test("does not run underlying validation for None") {
        for
          called <- Promise.make[Nothing, Unit]
          raw = Validation.instance[String](_ => called.succeed(()) *> ZIO.succeed("ok"))
          _         <- raw.optional.run(None)
          wasCalled <- called.isDone
        yield assertTrue(!wasCalled)
      }
    }
  }

  private case class NameInput(name: Option[String])
}
