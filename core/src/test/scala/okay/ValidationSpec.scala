package okay

import cats.implicits._
import munit.FunSuite
import okay.Violations.Path
import okay.violation.Violation
import okay.violation.implicits._

class ValidationSpec extends FunSuite {
  import ValidationSpec._

  test("at() lifts violations at specified path") {
    val validation = Validation.identity[Option[String]]().as[String].at("key-1")
    val result     = validation.validate(None)
    assertEquals(result, Violations[Violation](Violation.Required :: Nil).asChild("key-1").asLeft)
  }

  test("product() can make validation for a case class") {

    implicit val childValidation = Validation.forProduct[Violation, Dirty.Child](
      _.map(_.name).as[String].at("name"),
    ) { name =>
      Clean.Child(name = name)
    }

    val validation = Validation
      .forProduct[Violation, Dirty](
        _.map(_.a1).as[String].at("a1"),
        _.map(_.a2).as[Int].at("a2"),
        _.map(_.a3).as[List[Clean.Child]].at("a3"),
      ) { case (a1, a2, a3) =>
        Clean(a1 = a1, a2 = a2, a3 = a3)
      }

    assertEquals(
      validation.validate(
        Dirty(
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
        ),
      ),
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
  )

  object Dirty {
    case class Child(name: Option[String])
  }

  case class Clean(
    a1: String,
    a2: Int,
    a3: List[Clean.Child],
  )

  object Clean {
    case class Child(name: String)
  }
}
