package okay

import okay.defaults.{given, *}
import zio.Chunk
import zio.NonEmptyChunk
import zio.prelude.NonEmptyList
import zio.prelude.NonEmptySet
import zio.test.*

object NonEmptySyntaxSpec extends ZIOSpecDefault {

  override def spec = suiteAll("Validation.nonEmpty*") {
    suiteAll("nonEmptyList") {
      suiteAll("given") {
        test("resolves via validateAs") {
          for result <- List(1, 2, 3).validateAs[NonEmptyList[Int]].at("items")
          yield assertTrue(result == NonEmptyList(1, 2, 3))
        }
        test("fails with Violation.Required on empty list") {
          for result <- List.empty[Int].validateAs[NonEmptyList[Int]].at("items").either
          yield assertTrue(result.is(_.left) == Violations.single(Violation.Required).asChild("items"))
        }
        test("transforms elements via validateAs") {
          for result <- List("1", "2", "3").validateAs[NonEmptyList[Int]].at("items")
          yield assertTrue(result == NonEmptyList(1, 2, 3))
        }
        test("fails when head element transformation fails") {
          for result <- List("abc", "2").validateAs[NonEmptyList[Int]].at("items").either
          yield assertTrue(result.is(_.left) == Violations.single(Violation.NonIntegerString("abc")).asChild("items"))
        }
        test("fails when tail element transformation fails") {
          for result <- List("1", "abc").validateAs[NonEmptyList[Int]].at("items").either
          yield assertTrue(result.is(_.left) == Violations.single(Violation.NonIntegerString("abc")).asChild("items"))
        }
        test("succeeds with single-element list") {
          for result <- List("42").validateAs[NonEmptyList[Int]].at("items")
          yield assertTrue(result == NonEmptyList(42))
        }
      }
      suiteAll("NonEmptyList element validation") {
        test("transforms elements") {
          for result <- NonEmptyList("1", "2", "3").validateAs[NonEmptyList[Int]].at("items")
          yield assertTrue(result == NonEmptyList(1, 2, 3))
        }
        test("fails with element violation on invalid element") {
          for result <- NonEmptyList("1", "abc").validateAs[NonEmptyList[Int]].at("items").either
          yield assertTrue(result.is(_.left) == Violations.single(Violation.NonIntegerString("abc")).asChild("items"))
        }
      }
      suiteAll("explicit error") {
        test("succeeds with non-empty list") {
          val v = Validation.nonEmptyList[String, Int]("must not be empty")
          for result <- v.run(List(1, 2, 3))
          yield assertTrue(result == NonEmptyList(1, 2, 3))
        }
        test("fails with caller-defined error on empty list") {
          val v = Validation.nonEmptyList[String, Int]("must not be empty")
          for result <- v.run(List.empty[Int]).either
          yield assertTrue(result.is(_.left) == Violations.single("must not be empty"))
        }
      }
    }
    suiteAll("nonEmptyChunk") {
      test("resolves via validateAs") {
        for result <- Chunk(1, 2, 3).validateAs[NonEmptyChunk[Int]].at("data")
        yield assertTrue(result == NonEmptyChunk(1, 2, 3))
      }
      test("fails with Violation.Required on empty chunk") {
        for result <- Chunk.empty[Int].validateAs[NonEmptyChunk[Int]].at("data").either
        yield assertTrue(result.is(_.left) == Violations.single(Violation.Required).asChild("data"))
      }
    }
    suiteAll("nonEmptySet") {
      test("resolves via validateAs") {
        for result <- Set(1, 2, 3).validateAs[NonEmptySet[Int]].at("tags")
        yield assertTrue(result == NonEmptySet(1, 2, 3))
      }
      test("fails with Violation.Required on empty set") {
        for result <- Set.empty[Int].validateAs[NonEmptySet[Int]].at("tags").either
        yield assertTrue(result.is(_.left) == Violations.single(Violation.Required).asChild("tags"))
      }
    }
  }
}
