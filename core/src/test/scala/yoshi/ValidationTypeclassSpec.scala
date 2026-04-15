package yoshi

import yoshi.Violations.Path
import yoshi.defaults.*
import zio.test.*

object ValidationTypeclassSpec extends ZIOSpecDefault {
  override def spec = suiteAll("Validation typeclass instances") {
    suiteAll("optionCanBeValidatedAs") {
      test("derive Option validation from given") {
        given Validation[Violation, String, String] = Validations.minLength(1)
        val v                                       = summon[Validation[Violation, Option[String], Option[String]]]

        assertTrue(v.run(Some("hello")) == Right(Some("hello")))
      }
      test("pass through None") {
        given Validation[Violation, String, String] = Validations.minLength(1)
        val v                                       = summon[Validation[Violation, Option[String], Option[String]]]

        assertTrue(v.run(None) == Right(None))
      }
      test("fail when Some value is invalid") {
        given Validation[Violation, String, String] = Validations.minLength(5)
        val v                                       = summon[Validation[Violation, Option[String], Option[String]]]

        assertTrue(v.run(Some("ab")) == Left(Violations.of(Violation.TooShortString("ab", 5))))
      }
    }
    suiteAll("seqCanBeValidatedAs") {
      test("validate all elements in Seq") {
        given Validation[Violation, String, String] = Validations.minLength(1)
        val v                                       = summon[Validation[Violation, Seq[String], Seq[String]]]

        assertTrue(v.run(Seq("a", "bb", "ccc")) == Right(Seq("a", "bb", "ccc")))
      }
      test("succeed with empty Seq") {
        given Validation[Violation, String, String] = Validations.minLength(1)
        val v                                       = summon[Validation[Violation, Seq[String], Seq[String]]]

        assertTrue(v.run(Seq.empty) == Right(Seq.empty))
      }
      test("accumulate violations with indices") {
        given Validation[Violation, String, String] = Validations.minLength(3)
        val v                                       = summon[Validation[Violation, Seq[String], Seq[String]]]

        val expectedViolations = Violations[Violation](
          children = Map(
            Path(0) -> Violations(Vector(Violation.TooShortString("ab", 3))),
            Path(2) -> Violations(Vector(Violation.TooShortString("x", 3))),
          ),
        )

        assertTrue(v.run(Seq("ab", "hello", "x")) == Left(expectedViolations))
      }
    }
    suiteAll("listCanBeValidatedAs") {
      test("validate all elements in List") {
        given Validation[Violation, String, String] = Validations.minLength(1)
        val v                                       = summon[Validation[Violation, List[String], List[String]]]

        assertTrue(v.run(List("a", "bb", "ccc")) == Right(List("a", "bb", "ccc")))
      }
      test("accumulate violations with indices") {
        given Validation[Violation, String, String] = Validations.minLength(3)
        val v                                       = summon[Validation[Violation, List[String], List[String]]]

        val expectedViolations = Violations[Violation](
          children = Map(
            Path(0) -> Violations(Vector(Violation.TooShortString("ab", 3))),
            Path(2) -> Violations(Vector(Violation.TooShortString("x", 3))),
          ),
        )

        assertTrue(v.run(List("ab", "hello", "x")) == Left(expectedViolations))
      }
    }
    suiteAll("mapCanBeValidatedAs") {
      test("validate all values in Map") {
        given Validation[Violation, String, String] = Validations.minLength(1)
        val v                                       = summon[Validation[Violation, Map[String, String], Map[String, String]]]
        val input                                   = Map(
          "a" -> "x",
          "b" -> "yy",
        )

        assertTrue(v.run(input) == Right(input))
      }
      test("accumulate violations with keys") {
        given Validation[Violation, String, String] = Validations.minLength(3)
        val v                                       = summon[Validation[Violation, Map[String, String], Map[String, String]]]

        val expectedViolations = Violations[Violation](
          children = Map(
            Path("a") -> Violations(Vector(Violation.TooShortString("ab", 3))),
            Path("c") -> Violations(Vector(Violation.TooShortString("x", 3))),
          ),
        )

        assertTrue(v.run(Map("a" -> "ab", "b" -> "hello", "c" -> "x")) == Left(expectedViolations))
      }
    }
  }
}
