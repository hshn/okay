package okay

import okay.Violations.Path
import okay.Violations.Paths
import zio.test.ZIOSpecDefault
import zio.test.assertTrue

object ViolationsSpec extends ZIOSpecDefault {
  override def spec = suiteAll("Violations") {
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
    }
    test("map transforms values") {
      val v = Violations(values = Vector(1, 2, 3))
      assertTrue(v.map(_ * 10) == Violations(values = Vector(10, 20, 30)))
    }
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
    }
    test("map on empty returns empty") {
      val v = Violations.empty[Int]
      assertTrue(v.map(_.toString) == Violations.empty[String])
    }
    suiteAll("toList") {
      test("empty Violations returns empty list") {
        assertTrue(Violations.empty[String].toList == Nil)
      }
      test("flat Violations returns values with empty paths") {
        val violations = Violations(values = Vector("a", "b"))
        assertTrue(
          violations.toList == List(
            (Paths.empty, "a"),
            (Paths.empty, "b"),
          ),
        )
      }
      test("nested Violations returns values with paths") {
        val violations = Violations[String](
          children = Map(
            Path.Key("name") -> Violations(values = Vector("required")),
          ),
        )
        assertTrue(
          violations.toList == List(
            (Paths(List(Path.Key("name"))), "required"),
          ),
        )
      }
      test("deeply nested Violations returns full paths") {
        val violations = Violations[String](
          children = Map(
            Path.Key("address") -> Violations[String](
              children = Map(
                Path.Key("zip") -> Violations(values = Vector("invalid")),
              ),
            ),
          ),
        )
        assertTrue(
          violations.toList == List(
            (Paths(List(Path.Key("address"), Path.Key("zip"))), "invalid"),
          ),
        )
      }
      test("mixed root and nested values are all included") {
        val violations = Violations(
          values = Vector("root-error"),
          children = Map(
            Path.Key("field") -> Violations(values = Vector("field-error")),
            Path.Index(0)     -> Violations(values = Vector("index-error")),
          ),
        )
        val result = violations.toList
        assertTrue(
          result.contains((Paths.empty, "root-error")),
          result.contains((Paths(List(Path.Key("field"))), "field-error")),
          result.contains((Paths(List(Path.Index(0))), "index-error")),
          result.length == 3,
        )
      }
    }
    suiteAll("Paths#toString") {
      test("empty paths") {
        assertTrue(Paths.empty.toString == "")
      }
      test("single key") {
        assertTrue(Paths(List(Path.Key("name"))).toString == "name")
      }
      test("nested keys") {
        assertTrue(Paths(List(Path.Key("address"), Path.Key("zip"))).toString == "address.zip")
      }
      test("single index") {
        assertTrue(Paths(List(Path.Index(0))).toString == "[0]")
      }
      test("key followed by index") {
        assertTrue(Paths(List(Path.Key("items"), Path.Index(0))).toString == "items[0]")
      }
      test("key, index, key") {
        assertTrue(Paths(List(Path.Key("items"), Path.Index(0), Path.Key("name"))).toString == "items[0].name")
      }
      test("index followed by key") {
        assertTrue(Paths(List(Path.Index(0), Path.Key("name"))).toString == "[0].name")
      }
    }
  }
}
