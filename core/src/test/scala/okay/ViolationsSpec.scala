package okay

import okay.Violations.Path
import zio.Scope
import zio.test.Spec
import zio.test.TestEnvironment
import zio.test.ZIOSpecDefault
import zio.test.assertTrue

class ViolationsSpec extends ZIOSpecDefault {
  override def spec: Spec[TestEnvironment with Scope, Any] = suite("Violations")(
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

      assertTrue(a ++ b == c)
    },
  )
}
