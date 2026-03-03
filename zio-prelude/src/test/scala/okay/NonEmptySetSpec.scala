package okay

import okay.defaults.{given, *}
import okay.prelude.given
import zio.prelude.NonEmptySet
import zio.test.*

object NonEmptySetSpec extends ZIOSpecDefault {

  override def spec = suiteAll("NonEmptySet") {
    suiteAll("Set → NonEmptySet") {
      test("succeeds with non-empty set") {
        for result <- Set(1, 2, 3).validateAs[NonEmptySet[Int]]
        yield assertTrue(result == NonEmptySet(1, 2, 3))
      }
      test("fails with Violation.Required on empty set") {
        for result <- Set.empty[Int].validateAs[NonEmptySet[Int]].either
        yield assertTrue(result.is(_.left) == Violations.single(Violation.Required))
      }
      test("succeeds with single element") {
        for result <- Set(42).validateAs[NonEmptySet[Int]]
        yield assertTrue(result == NonEmptySet(42))
      }
    }
    suiteAll("Set → NonEmptySet (element transform)") {
      test("transforms all elements") {
        for result <- Set("1", "2", "3").validateAs[NonEmptySet[Int]]
        yield assertTrue(result == NonEmptySet(1, 2, 3))
      }
      test("fails only for invalid elements in a mixed set") {
        for result <- Set("abc", "2").validateAs[NonEmptySet[Int]].flip
        yield {
          val violations = result.toList.map(_._2)
          assertTrue(
            violations.length == 1,
            violations.contains(Violation.NonIntegerString("abc")),
          )
        }
      }
    }
    suiteAll("NonEmptySet element transform") {
      test("transforms all elements") {
        for result <- NonEmptySet("1", "2", "3").validateAs[NonEmptySet[Int]]
        yield assertTrue(result == NonEmptySet(1, 2, 3))
      }
      test("fails when element transformation fails") {
        for result <- NonEmptySet("abc").validateAs[NonEmptySet[Int]].flip
        yield {
          val violations = result.toList.map(_._2)
          assertTrue(
            violations.length == 1,
            violations.contains(Violation.NonIntegerString("abc")),
          )
        }
      }
      test("accumulates violations from multiple invalid elements") {
        for result <- NonEmptySet("abc", "def").validateAs[NonEmptySet[Int]].flip
        yield {
          val violations = result.toList.map(_._2)
          assertTrue(
            violations.length == 2,
            violations.contains(Violation.NonIntegerString("abc")),
            violations.contains(Violation.NonIntegerString("def")),
          )
        }
      }
    }
  }
}
