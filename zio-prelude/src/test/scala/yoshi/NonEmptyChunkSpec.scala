package yoshi

import yoshi.defaults.*
import yoshi.prelude.*
import zio.Chunk
import zio.NonEmptyChunk
import zio.test.*

object NonEmptyChunkSpec extends ZIOSpecDefault {

  override def spec = suiteAll("NonEmptyChunk") {
    suiteAll("Chunk → NonEmptyChunk") {
      test("succeeds with non-empty chunk") {
        for result <- Chunk(1, 2, 3).validateAs[NonEmptyChunk[Int]]
        yield assertTrue(result == NonEmptyChunk(1, 2, 3))
      }
      test("fails with Violation.Required on empty chunk") {
        for result <- Chunk.empty[Int].validateAs[NonEmptyChunk[Int]].either
        yield assertTrue(result.is(_.left) == Violations.of(Violation.Required))
      }
      test("succeeds with single element") {
        for result <- Chunk(42).validateAs[NonEmptyChunk[Int]]
        yield assertTrue(result == NonEmptyChunk(42))
      }
    }
    suiteAll("Chunk → NonEmptyChunk (element transform)") {
      test("transforms all elements") {
        for result <- Chunk("1", "2", "3").validateAs[NonEmptyChunk[Int]]
        yield assertTrue(result == NonEmptyChunk(1, 2, 3))
      }
      test("fails at index 0 when head transformation fails") {
        for result <- Chunk("abc", "2").validateAs[NonEmptyChunk[Int]].either
        yield assertTrue(result.is(_.left) == Violations.of(Violation.NonIntegerString("abc")).asChild(0))
      }
      test("fails at index 1 when tail transformation fails") {
        for result <- Chunk("1", "abc").validateAs[NonEmptyChunk[Int]].either
        yield assertTrue(result.is(_.left) == Violations.of(Violation.NonIntegerString("abc")).asChild(1))
      }
    }
    suiteAll("NonEmptyChunk element transform") {
      test("transforms all elements") {
        for result <- NonEmptyChunk("1", "2", "3").validateAs[NonEmptyChunk[Int]]
        yield assertTrue(result == NonEmptyChunk(1, 2, 3))
      }
      test("fails at the index of the invalid element") {
        for result <- NonEmptyChunk("1", "abc").validateAs[NonEmptyChunk[Int]].either
        yield assertTrue(result.is(_.left) == Violations.of(Violation.NonIntegerString("abc")).asChild(1))
      }
      test("accumulates violations from multiple invalid elements") {
        for result <- NonEmptyChunk("abc", "def").validateAs[NonEmptyChunk[Int]].either
        yield assertTrue(
          result.is(_.left) ==
            Violations.of(Violation.NonIntegerString("abc")).asChild(0) ++
            Violations.of(Violation.NonIntegerString("def")).asChild(1),
        )
      }
    }
  }
}
