package yoshi

import yoshi.defaults.*
import zio.test.*

object ValidationTransformsSpec extends ZIOSpecDefault {
  override def spec = suiteAll("Validation transforms") {
    suiteAll("succeed") {
      test("returns the input unchanged") {
        assertTrue(Validation.succeed[String].run("hello") == Right("hello"))
      }
      test("is right identity for >>") {
        val v = Validations.minLength(1)
        assertTrue((v >> Validation.succeed).run("ab") == Right("ab"))
      }
      test("is left identity for >>") {
        val v = Validations.minLength(1)
        assertTrue((Validation.succeed >> v).run("ab") == Right("ab"))
      }
    }
    suiteAll("fail") {
      test("always fails with the given violation") {
        val v = Validation.fail[Violation](Violation.Required)
        assertTrue(v.run("anything") == Left(Violations.of[Violation](Violation.Required)))
      }
    }
    suiteAll("contramap") {
      test("adapt input type before validation") {
        val nonEmpty: Validation[Violation, String, String]          = Validations.minLength(1)
        val nameValidation: Validation[Violation, NameInput, String] =
          nonEmpty.contramap(_.name.getOrElse(""))

        assertTrue(nameValidation.run(NameInput(name = Some("Alice"))) == Right("Alice"))
      }
      test("propagate violations from adapted input") {
        val minLength3: Validation[Violation, String, String]        = Validations.minLength(3)
        val nameValidation: Validation[Violation, NameInput, String] =
          minLength3.contramap(_.name.getOrElse(""))

        assertTrue(nameValidation.run(NameInput(name = Some("ab"))) == Left(Violations(Vector(Violation.TooShortString("ab", 3)))))
      }
    }
    suiteAll("mapError") {
      test("transform error type on failure") {
        val v = Validations.minLength(3).mapError(_.toString)

        assertTrue(v.run("ab") == Left(Violations.of(Violation.TooShortString("ab", 3).toString)))
      }
      test("preserve success value") {
        val v = Validations.minLength(1).mapError(_.toString)

        assertTrue(v.run("hello") == Right("hello"))
      }
      test("transform nested violations preserving structure") {
        val v = Validation
          .instance[String] { s =>
            Left(
              Violations[Violation](
                values = Vector(Violation.Required),
                children = Map(
                  Violations.Path("child") -> Violations.of(Violation.TooShortString(s, 3)),
                ),
              ),
            )
          }
          .mapError(_.toString)

        assertTrue(
          v.run("ab") == Left(
            Violations[String](
              values = Vector(Violation.Required.toString),
              children = Map(
                Violations.Path("child") -> Violations.of(Violation.TooShortString("ab", 3).toString),
              ),
            ),
          ),
        )
      }
      test("compose with >>") {
        val first: Validation[Violation, String, String]  = Validations.minLength(1)
        val second: Validation[Violation, String, String] = Validations.maxLength(3)
        val v                                             = (first >> second).mapError(_.toString)

        assertTrue(v.run("hello") == Left(Violations.of(Violation.TooLongString("hello", 3).toString)))
      }
      test("compose with |+|") {
        val v = (Validations.minLength(3) |+| Validations.maxLength(1)).mapError(_.toString)

        assertTrue(
          v.run("ab") == Left(
            Violations(
              Vector(
                Violation.TooShortString("ab", 3).toString,
                Violation.TooLongString("ab", 1).toString,
              ),
            ),
          ),
        )
      }
    }
    suiteAll("optional") {
      test("pass through None as success") {
        val v = Validations.minLength(3).optional
        assertTrue(v.run(None) == Right(None))
      }
      test("validate Some value and wrap result in Some") {
        val v = Validations.minLength(3).optional
        assertTrue(v.run(Some("hello")) == Right(Some("hello")))
      }
      test("fail when Some value is invalid") {
        val v = Validations.minLength(3).optional
        assertTrue(v.run(Some("ab")) == Left(Violations.of(Violation.TooShortString("ab", 3))))
      }
      test("compose with map") {
        val v = Validations.minLength(1).optional.map(_.map(_.length))
        assertTrue(v.run(Some("hello")) == Right(Some(5)))
      }
      test("compose with sequential >>") {
        val v = (Validations.minLength(1) >> Validations.maxLength(5)).optional
        assertTrue(v.run(Some("abc")) == Right(Some("abc")))
      }
      test("fail in sequential >> composition") {
        val v = (Validations.minLength(1) >> Validations.maxLength(3)).optional
        assertTrue(v.run(Some("hello")) == Left(Violations.of(Violation.TooLongString("hello", 3))))
      }
      test("does not run underlying validation for None") {
        var called = false
        val raw    = Validation.instance[String] { _ => called = true; Right("ok") }
        val _      = raw.optional.run(None)
        assertTrue(!called)
      }
    }
  }

  private case class NameInput(name: Option[String])
}
