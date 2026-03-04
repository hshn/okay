package okay

import okay.Violations.Path
import okay.defaults.*
import zio.test.*

object ValidationTypeclassSpec extends ZIOSpecDefault {
  override def spec = suiteAll("Validation typeclass instances") {
    suiteAll("optionCanBeValidatedAs") {
      test("derive Option validation from given") {
        given Validation[Any, Violation, String, String] = Validations.minLength(1)
        val v                                            = summon[Validation[Any, Violation, Option[String], Option[String]]]

        for result <- v.run(Some("hello"))
        yield assertTrue(result == Some("hello"))
      }
      test("pass through None") {
        given Validation[Any, Violation, String, String] = Validations.minLength(1)
        val v                                            = summon[Validation[Any, Violation, Option[String], Option[String]]]

        for result <- v.run(None)
        yield assertTrue(result == None)
      }
      test("fail when Some value is invalid") {
        given Validation[Any, Violation, String, String] = Validations.minLength(5)
        val v                                            = summon[Validation[Any, Violation, Option[String], Option[String]]]

        for result <- v.run(Some("ab")).either
        yield assertTrue(result.is(_.left) == Violations.single(Violation.TooShortString("ab", 5)))
      }
    }
    suiteAll("seqCanBeValidatedAs") {
      test("validate all elements in Seq") {
        given Validation[Any, Violation, String, String] = Validations.minLength(1)
        val v                                            = summon[Validation[Any, Violation, Seq[String], Seq[String]]]

        for result <- v.run(Seq("a", "bb", "ccc"))
        yield assertTrue(result == Seq("a", "bb", "ccc"))
      }
      test("succeed with empty Seq") {
        given Validation[Any, Violation, String, String] = Validations.minLength(1)
        val v                                            = summon[Validation[Any, Violation, Seq[String], Seq[String]]]

        for result <- v.run(Seq.empty)
        yield assertTrue(result == Seq.empty)
      }
      test("accumulate violations with indices") {
        given Validation[Any, Violation, String, String] = Validations.minLength(3)
        val v                                            = summon[Validation[Any, Violation, Seq[String], Seq[String]]]

        val expectedViolations = Violations[Violation](
          children = Map(
            Path(0) -> Violations(Vector(Violation.TooShortString("ab", 3))),
            Path(2) -> Violations(Vector(Violation.TooShortString("x", 3))),
          ),
        )

        for result <- v.run(Seq("ab", "hello", "x")).either
        yield assertTrue(result.is(_.left) == expectedViolations)
      }
    }
    suiteAll("listCanBeValidatedAs") {
      test("validate all elements in List") {
        given Validation[Any, Violation, String, String] = Validations.minLength(1)
        val v                                            = summon[Validation[Any, Violation, List[String], List[String]]]

        for result <- v.run(List("a", "bb", "ccc"))
        yield assertTrue(result == List("a", "bb", "ccc"))
      }
      test("accumulate violations with indices") {
        given Validation[Any, Violation, String, String] = Validations.minLength(3)
        val v                                            = summon[Validation[Any, Violation, List[String], List[String]]]

        val expectedViolations = Violations[Violation](
          children = Map(
            Path(0) -> Violations(Vector(Violation.TooShortString("ab", 3))),
            Path(2) -> Violations(Vector(Violation.TooShortString("x", 3))),
          ),
        )

        for result <- v.run(List("ab", "hello", "x")).either
        yield assertTrue(result.is(_.left) == expectedViolations)
      }
    }
    suiteAll("mapCanBeValidatedAs") {
      test("validate all values in Map") {
        given Validation[Any, Violation, String, String] = Validations.minLength(1)
        val v = summon[Validation[Any, Violation, Map[String, String], Map[String, String]]]
        val input = Map(
          "a" -> "x",
          "b" -> "yy",
        )

        for result <- v.run(input)
        yield assertTrue(result == input)
      }
      test("accumulate violations with keys") {
        given Validation[Any, Violation, String, String] = Validations.minLength(3)
        val v = summon[Validation[Any, Violation, Map[String, String], Map[String, String]]]

        val expectedViolations = Violations[Violation](
          children = Map(
            Path("a") -> Violations(Vector(Violation.TooShortString("ab", 3))),
            Path("c") -> Violations(Vector(Violation.TooShortString("x", 3))),
          ),
        )

        for result <- v.run(Map("a" -> "ab", "b" -> "hello", "c" -> "x")).either
        yield assertTrue(result.is(_.left) == expectedViolations)
      }
    }
  }
}
