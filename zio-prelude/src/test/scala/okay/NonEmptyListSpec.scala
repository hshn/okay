package okay

import okay.defaults.{given, *}
import zio.prelude.NonEmptyList
import zio.test.*

object NonEmptyListSpec extends ZIOSpecDefault {

  override def spec = suiteAll("NonEmptyList") {
    suiteAll("List → NonEmptyList") {
      test("succeeds with non-empty list") {
        for result <- List(1, 2, 3).validateAs[NonEmptyList[Int]]
        yield assertTrue(result == NonEmptyList(1, 2, 3))
      }
      test("fails with Violation.Required on empty list") {
        for result <- List.empty[Int].validateAs[NonEmptyList[Int]].either
        yield assertTrue(result.is(_.left) == Violations.single(Violation.Required))
      }
      test("succeeds with single element") {
        for result <- List(42).validateAs[NonEmptyList[Int]]
        yield assertTrue(result == NonEmptyList(42))
      }
    }
    suiteAll("List → NonEmptyList (element transform)") {
      test("transforms all elements") {
        for result <- List("1", "2", "3").validateAs[NonEmptyList[Int]]
        yield assertTrue(result == NonEmptyList(1, 2, 3))
      }
      test("fails at index 0 when head transformation fails") {
        for result <- List("abc", "2").validateAs[NonEmptyList[Int]].either
        yield assertTrue(result.is(_.left) == Violations.single(Violation.NonIntegerString("abc")).asChild(0))
      }
      test("fails at index 1 when tail transformation fails") {
        for result <- List("1", "abc").validateAs[NonEmptyList[Int]].either
        yield assertTrue(result.is(_.left) == Violations.single(Violation.NonIntegerString("abc")).asChild(1))
      }
    }
    suiteAll("NonEmptyList element transform") {
      test("transforms all elements") {
        for result <- NonEmptyList("1", "2", "3").validateAs[NonEmptyList[Int]]
        yield assertTrue(result == NonEmptyList(1, 2, 3))
      }
      test("fails at the index of the invalid element") {
        for result <- NonEmptyList("1", "abc").validateAs[NonEmptyList[Int]].either
        yield assertTrue(result.is(_.left) == Violations.single(Violation.NonIntegerString("abc")).asChild(1))
      }
      test("accumulates violations from multiple invalid elements") {
        for result <- NonEmptyList("abc", "def").validateAs[NonEmptyList[Int]].either
        yield assertTrue(
          result.is(_.left) ==
            Violations.single(Violation.NonIntegerString("abc")).asChild(0) ++
            Violations.single(Violation.NonIntegerString("def")).asChild(1),
        )
      }
    }
  }
}
