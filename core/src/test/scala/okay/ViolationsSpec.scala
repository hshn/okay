package okay

import okay.Violations.Path
import zio.Scope
import zio.test.Spec
import zio.test.TestEnvironment
import zio.test.ZIOSpecDefault
import zio.test.assertTrue

object ViolationsSpec extends ZIOSpecDefault {
  override def spec: Spec[TestEnvironment with Scope, Any] = suite("Violations")(
    test("++ merges two Violations recursively") {
      val a = Violations(
        values = Vector("a", "b"),
        children = Map(
          Path.Key("c") -> Violations(
            values = Vector("d"),
          ),
          Path.Key("e") -> Violations(
            values = Vector("f"),
          ),
          Path.Index(1) -> Violations(
            values = Vector("g"),
          ),
          Path.Index(2) -> Violations(
            values = Vector("h"),
          ),
        ),
      )
      val b = Violations(
        values = Vector("b", "c"),
        children = Map(
          Path.Key("e") -> Violations(
            values = Vector("f"),
          ),
          Path.Key("g") -> Violations(
            values = Vector("h"),
          ),
          Path.Index(2) -> Violations(
            values = Vector("i"),
          ),
          Path.Index(3) -> Violations(
            values = Vector("j"),
          ),
        ),
      )

      val c = Violations(
        values = Vector("a", "b", "b", "c"),
        children = Map(
          Path.Key("c") -> Violations(
            values = Vector("d"),
          ),
          Path.Key("e") -> Violations(
            values = Vector("f", "f"),
          ),
          Path.Key("g") -> Violations(
            values = Vector("h"),
          ),
          Path.Index(1) -> Violations(
            values = Vector("g"),
          ),
          Path.Index(2) -> Violations(
            values = Vector("h", "i"),
          ),
          Path.Index(3) -> Violations(
            values = Vector("j"),
          ),
        ),
      )

      assertTrue(a ++ b == c)
    },
    test("map transforms values") {
      val v = Violations(values = Vector(1, 2, 3))
      assertTrue(v.map(_ * 10) == Violations(values = Vector(10, 20, 30)))
    },
    test("map transforms children recursively") {
      val v = Violations(
        values = Vector(1),
        children = Map(
          Path.Key("a") -> Violations(
            values = Vector(2),
            children = Map(Path.Index(0) -> Violations(values = Vector(3))),
          ),
        ),
      )
      val expected = Violations(
        values = Vector("1"),
        children = Map(
          Path.Key("a") -> Violations(
            values = Vector("2"),
            children = Map(Path.Index(0) -> Violations(values = Vector("3"))),
          ),
        ),
      )
      assertTrue(v.map(_.toString) == expected)
    },
    test("map on empty returns empty") {
      val v = Violations.empty[Int]
      assertTrue(v.map(_.toString) == Violations.empty[String])
    },
  )
}
