package yoshi

import yoshi.defaults.*
import zio.test.*

object CursorSpec extends ZIOSpecDefault {

  case class Input(
    name: Option[String],
    age: String,
    items: List[Input.Item],
    address: Input.Address,
  )

  object Input:
    case class Item(label: Option[String])
    case class Address(zip: Option[String])

  case class Output(
    name: String,
    age: Int,
    items: List[Output.Item],
  )

  object Output:
    case class Item(label: String)

  given Validation[Violation, Input.Item, Output.Item] =
    Validation.cursor[Input.Item] { c =>
      (
        c.field(_.label).validateAs[String]
      ).validateN { label =>
        Output.Item(label)
      }
    }

  val validation: Validation[Violation, Input, Output] =
    Validation.cursor[Input] { c =>
      (
        c.field(_.name).validateAs[String],
        c.field(_.age).validateAs[Int],
        c.field(_.items).validateAs[List[Output.Item]],
      ).validateN { case (name, age, items) =>
        Output(name, age, items)
      }
    }

  override def spec = suiteAll("Validation.cursor") {
    suiteAll("field") {
      test("transforms valid input") {
        val input = Input(
          name = Some("Alice"),
          age = "30",
          items = List(Input.Item(Some("item1")), Input.Item(Some("item2"))),
          address = Input.Address(Some("12345")),
        )
        val expected = Output(
          name = "Alice",
          age = 30,
          items = List(Output.Item("item1"), Output.Item("item2")),
        )

        assertTrue(validation.run(input) == Right(expected))
      }

      test("accumulates violations with correct paths") {
        val input = Input(
          name = None,
          age = "abc",
          items = List(Input.Item(Some("ok")), Input.Item(None)),
          address = Input.Address(None),
        )

        val expected = Violations[Violation](
          children = Map(
            Violations.Path("name")  -> Violations(Vector(Violation.Required)),
            Violations.Path("age")   -> Violations(Vector(Violation.NonIntegerString("abc"))),
            Violations.Path("items") -> Violations(
              children = Map(
                Violations.Path(1) -> Violations(Vector(Violation.Required)).asChild("label"),
              ),
            ),
          ),
        )

        assertTrue(validation.run(input) == Left(expected))
      }

      test("produces same result as manual .at() calls") {
        val manualValidation: Validation[Violation, Input, Output] =
          Validation.instance[Input] { dirty =>
            (
              dirty.name.validateAs[String].at("name"),
              dirty.age.validateAs[Int].at("age"),
              dirty.items.validateAs[List[Output.Item]].at("items"),
            ).validateN { case (name, age, items) =>
              Output(name, age, items)
            }
          }

        val invalid = Input(name = None, age = "xyz", items = List(Input.Item(None)), address = Input.Address(None))

        val cursorResult = validation.run(invalid)
        val manualResult = manualValidation.run(invalid)
        assertTrue(cursorResult == manualResult)
      }
    }

    suiteAll("nested field access") {
      test("derives full path from nested accessor") {
        val v: Validation[Violation, Input, String] =
          Validation.cursor[Input] { c =>
            c.field(_.address.zip).validateAs[String]
          }

        val input = Input(name = None, age = "", items = Nil, address = Input.Address(None))

        val expected = Violations[Violation](
          children = Map(
            Violations.Path("address") -> Violations(
              children = Map(
                Violations.Path("zip") -> Violations(Vector(Violation.Required)),
              ),
            ),
          ),
        )
        assertTrue(v.run(input) == Left(expected))
      }
    }

    suiteAll("field with .at() override") {
      test("uses overridden path instead of derived name") {
        val v: Validation[Violation, Input, String] =
          Validation.cursor[Input] { c =>
            c.field(_.name).at("display_name").validateAs[String]
          }

        val expected = Violations[Violation](
          children = Map(
            Violations.Path("display_name") -> Violations(Vector(Violation.Required)),
          ),
        )
        assertTrue(v.run(Input(name = None, age = "", items = Nil, address = Input.Address(None))) == Left(expected))
      }
    }

    suiteAll("validateAs shorthand") {
      test("produces same result as field().validateAs") {
        val withField: Validation[Violation, Input, String] =
          Validation.cursor[Input] { c =>
            c.field(_.name).validateAs[String]
          }

        val withShorthand: Validation[Violation, Input, String] =
          Validation.cursor[Input] { c =>
            c.validateAs[String](_.name)
          }

        val invalid = Input(name = None, age = "", items = Nil, address = Input.Address(None))

        val fieldResult     = withField.run(invalid)
        val shorthandResult = withShorthand.run(invalid)
        assertTrue(fieldResult == shorthandResult)
      }
    }
  }
}
