package okay

import munit.FunSuite
import okay.Violations.Path

class ViolationsSpec extends FunSuite {
  test("++ merges two Violations recursively") {
    val a = Violations(
      values = Seq("a", "b"),
      children = Map(
        Path.Key("c") -> Violations(
          values = Seq("d"),
        ),
        Path.Key("e") -> Violations(
          values = Seq("f"),
        ),
        Path.Index(1) -> Violations(
          values = Seq("g"),
        ),
        Path.Index(2) -> Violations(
          values = Seq("h"),
        ),
      ),
    )
    val b = Violations(
      values = Seq("b", "c"),
      children = Map(
        Path.Key("e") -> Violations(
          values = Seq("f"),
        ),
        Path.Key("g") -> Violations(
          values = Seq("h"),
        ),
        Path.Index(2) -> Violations(
          values = Seq("i"),
        ),
        Path.Index(3) -> Violations(
          values = Seq("j"),
        ),
      ),
    )

    val c = Violations(
      values = Seq("a", "b", "b", "c"),
      children = Map(
        Path.Key("c") -> Violations(
          values = Seq("d"),
        ),
        Path.Key("e") -> Violations(
          values = Seq("f", "f"),
        ),
        Path.Key("g") -> Violations(
          values = Seq("h"),
        ),
        Path.Index(1) -> Violations(
          values = Seq("g"),
        ),
        Path.Index(2) -> Violations(
          values = Seq("h", "i"),
        ),
        Path.Index(3) -> Violations(
          values = Seq("j"),
        ),
      ),
    )

    assertEquals(a ++ b, c)
  }
}
