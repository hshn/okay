package okay

import cats.implicits._
import munit.FunSuite
import okay.Violations.Path
import okay.defaults._
import okay.defaults.Validations._

class ValidationSpec extends FunSuite {
  import ValidationSpec._

  test("at() lifts violations at specified path") {
    val validation = Validation.okay[Option[String]].as[String]().at("key-1")
    val result     = validation.validate(None)
    assertEquals(result, Violations[Violation](Violation.Required :: Nil).asChild("key-1").asLeft)
  }

  test("product() can make validation for a case class") {
    implicit val childValidation: Validation[Violation, Dirty.Child, Clean.Child] =
      Validation.forProduct[Dirty.Child](
        _.map(_.name).as[String]().at("name"),
      ) { name =>
        Clean.Child(name = name)
      }

    val validation = Validation
      .forProduct[Dirty](
        _.map(_.a1).as[String]().at("a1"),
        _.map(_.a2).as[Int]().at("a2"),
        _.map(_.a3).as[List[Clean.Child]]().at("a3"),
        _.map(_.a4).as[Map[String, Clean.Child]]().at("a4"),
      ) { case (a1, a2, a3, a4) =>
        Clean(a1 = a1, a2 = a2, a3 = a3, a4 = a4)
      }

    val invalid = Dirty(
      a1 = None,
      a2 = "yay",
      a3 = List(
        Dirty.Child(name = "0".some),
        Dirty.Child(name = None),
        Dirty.Child(name = "2".some),
        Dirty.Child(name = "3".some),
        Dirty.Child(name = None),
        Dirty.Child(name = "5".some),
      ),
      a4 = Map(
        "a" -> Dirty.Child(name = None),
        "b" -> Dirty.Child(name = "1".some),
        "c" -> Dirty.Child(name = None),
        "d" -> Dirty.Child(name = None),
        "e" -> Dirty.Child(name = "4".some),
        "f" -> Dirty.Child(name = None),
      ),
    )

    val valid = Dirty(
      a1 = "hi".some,
      a2 = "123",
      a3 = List(
        Dirty.Child(name = "0".some),
        Dirty.Child(name = "2".some),
        Dirty.Child(name = "3".some),
        Dirty.Child(name = "5".some),
      ),
      a4 = Map(
        "b" -> Dirty.Child(name = "1".some),
        "e" -> Dirty.Child(name = "4".some),
      ),
    )

    assertEquals(
      validation.validate(invalid),
      Violations[Violation](
        children = Map(
          Path("a1") -> Violations(Violation.Required :: Nil),
          Path("a2") -> Violations(Violation.NonIntegerString("yay") :: Nil),
          Path("a3") -> Violations(
            children = Map(
              Path(1) -> Violations(Violation.Required :: Nil).asChild("name"),
              Path(4) -> Violations(Violation.Required :: Nil).asChild("name"),
            ),
          ),
          Path("a4") -> Violations(
            children = Map(
              Path("a") -> Violations(Violation.Required :: Nil).asChild("name"),
              Path("c") -> Violations(Violation.Required :: Nil).asChild("name"),
              Path("d") -> Violations(Violation.Required :: Nil).asChild("name"),
              Path("f") -> Violations(Violation.Required :: Nil).asChild("name"),
            ),
          ),
        ),
      ).asLeft,
    )

    assertEquals(
      validation.validate(valid),
      Clean(
        a1 = "hi",
        a2 = 123,
        a3 = List(
          Clean.Child(name = "0"),
          Clean.Child(name = "2"),
          Clean.Child(name = "3"),
          Clean.Child(name = "5"),
        ),
        a4 = Map(
          "b" -> Clean.Child(name = "1"),
          "e" -> Clean.Child(name = "4"),
        ),
      ).asRight,
    )
  }

  test("|+| can combine two validation horizontally") {
    val validation = maxLength(max = 10) |+|
      minLength(min = 20) |+|
      minLength(min = 30) |+|
      maxLength(max = 100)

    val string = "a".repeat(15)

    assertEquals(
      validation.validate(string),
      Violations[Violation](
        Seq(
          Violation.TooLongString(value = string, maxLength = 10),
          Violation.TooShortString(value = string, minLength = 20),
          Violation.TooShortString(value = string, minLength = 30),
        ),
      ).asLeft,
    )
  }
}

object ValidationSpec {

  case class Dirty(
    a1: Option[String],
    a2: String,
    a3: List[Dirty.Child],
    a4: Map[String, Dirty.Child],
  )

  object Dirty {
    case class Child(name: Option[String])
  }

  case class Clean(
    a1: String,
    a2: Int,
    a3: List[Clean.Child],
    a4: Map[String, Clean.Child],
  )

  object Clean {
    case class Child(name: String)
  }
}
