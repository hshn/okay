package yoshi

import yoshi.defaults.*
import yoshi.prelude.*
import zio.prelude.NonEmptySet
import zio.test.*

object NonEmptySetSpec extends ZIOSpecDefault {

  override def spec = suiteAll("NonEmptySet") {
    suiteAll("Set → NonEmptySet") {
      test("succeeds with non-empty set") {
        for {
          result <- Set(1, 2, 3).validateAs[NonEmptySet[Int]]
        } yield {
          assertTrue(result == NonEmptySet(1, 2, 3))
        }
      }
      test("fails with Violation.Required on empty set") {
        assertTrue(Set.empty[Int].validateAs[NonEmptySet[Int]].is(_.left) == Violations.of(Violation.Required))
      }
      test("succeeds with single element") {
        for {
          result <- Set(42).validateAs[NonEmptySet[Int]]
        } yield {
          assertTrue(result == NonEmptySet(42))
        }
      }
    }
    suiteAll("Set → NonEmptySet (element transform)") {
      test("transforms all elements") {
        for {
          result <- Set("1", "2", "3").validateAs[NonEmptySet[Int]]
        } yield {
          assertTrue(result == NonEmptySet(1, 2, 3))
        }
      }
      test("fails only for invalid elements in a mixed set") {
        val Left(result) = Set("abc", "2").validateAs[NonEmptySet[Int]]: @unchecked
        val violations   = result.toList.map(_._2)
        assertTrue(
          violations.length == 1,
          violations.contains(Violation.NonIntegerString("abc")),
        )
      }
    }
    suiteAll("NonEmptySet element transform") {
      test("transforms all elements") {
        for {
          result <- NonEmptySet("1", "2", "3").validateAs[NonEmptySet[Int]]
        } yield {
          assertTrue(result == NonEmptySet(1, 2, 3))
        }
      }
      test("fails when element transformation fails") {
        val Left(result) = NonEmptySet("abc").validateAs[NonEmptySet[Int]]: @unchecked
        val violations   = result.toList.map(_._2)
        assertTrue(
          violations.length == 1,
          violations.contains(Violation.NonIntegerString("abc")),
        )
      }
      test("accumulates violations from multiple invalid elements") {
        val Left(result) = NonEmptySet("abc", "def").validateAs[NonEmptySet[Int]]: @unchecked
        val violations   = result.toList.map(_._2)
        assertTrue(
          violations.length == 2,
          violations.contains(Violation.NonIntegerString("abc")),
          violations.contains(Violation.NonIntegerString("def")),
        )
      }
    }
  }
}
