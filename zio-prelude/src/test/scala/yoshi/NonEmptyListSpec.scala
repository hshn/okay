package yoshi

import yoshi.defaults.*
import yoshi.prelude.*
import zio.prelude.NonEmptyList
import zio.test.*

object NonEmptyListSpec extends ZIOSpecDefault {

  override def spec = suiteAll("NonEmptyList") {
    suiteAll("List → NonEmptyList") {
      test("succeeds with non-empty list") {
        assertTrue(List(1, 2, 3).validateAs[NonEmptyList[Int]].is(_.right) == NonEmptyList(1, 2, 3))
      }
      test("fails with Violation.Required on empty list") {
        assertTrue(List.empty[Int].validateAs[NonEmptyList[Int]].is(_.left) == Violations.of(Violation.Required))
      }
      test("succeeds with single element") {
        assertTrue(List(42).validateAs[NonEmptyList[Int]].is(_.right) == NonEmptyList(42))
      }
    }
    suiteAll("List → NonEmptyList (element transform)") {
      test("transforms all elements") {
        assertTrue(List("1", "2", "3").validateAs[NonEmptyList[Int]].is(_.right) == NonEmptyList(1, 2, 3))
      }
      test("fails at index 0 when head transformation fails") {
        assertTrue(
          List("abc", "2").validateAs[NonEmptyList[Int]].is(_.left) ==
            Violations.of(Violation.NonIntegerString("abc")).asChild(0),
        )
      }
      test("fails at index 1 when tail transformation fails") {
        assertTrue(
          List("1", "abc").validateAs[NonEmptyList[Int]].is(_.left) ==
            Violations.of(Violation.NonIntegerString("abc")).asChild(1),
        )
      }
    }
    suiteAll("NonEmptyList element transform") {
      test("transforms all elements") {
        assertTrue(NonEmptyList("1", "2", "3").validateAs[NonEmptyList[Int]].is(_.right) == NonEmptyList(1, 2, 3))
      }
      test("fails at the index of the invalid element") {
        assertTrue(
          NonEmptyList("1", "abc").validateAs[NonEmptyList[Int]].is(_.left) ==
            Violations.of(Violation.NonIntegerString("abc")).asChild(1),
        )
      }
      test("accumulates violations from multiple invalid elements") {
        assertTrue(
          NonEmptyList("abc", "def").validateAs[NonEmptyList[Int]].is(_.left) ==
            Violations.of(Violation.NonIntegerString("abc")).asChild(0) ++
            Violations.of(Violation.NonIntegerString("def")).asChild(1),
        )
      }
    }
  }
}
