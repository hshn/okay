package okay

import cats.implicits._
import okay.Validation._
import okay.Violations.Path
import okay.defaults._
import okay.defaults.Validations._
import zio.Scope
import zio.test.Assertion._
import zio.test.Spec
import zio.test.TestEnvironment
import zio.test.ZIOSpecDefault
import zio.test.assertZIO

object ValidationSpec extends ZIOSpecDefault {
  implicit val childValidation: Validation[Any, Violation, Dirty.Child, Clean.Child] =
    Validation.forProduct[Dirty.Child](
      _.map(_.name) >> required at "name",
    ) { name =>
      Clean.Child(name = name)
    }

  val validation: Validation[Any, Violation, Dirty, Clean] =
    Validation.forProduct[Dirty](
      _.map(_.a1) >> required at "a1",
      _.map(_.a2) >> as[Int]() at "a2",
      _.map(_.a3) >> as[List[Clean.Child]]() at "a3",
      _.map(_.a4) >> as[Map[String, Clean.Child]]() at "a4",
    ) { case (a1, a2, a3, a4) =>
      Clean(
        a1 = a1,
        a2 = a2,
        a3 = a3,
        a4 = a4,
      )
    }

  val suiteCombine = suite("|+|")(
    test("result violations when invalid") {
      val validation = Validations.minLength(3) |+| Validations.maxLength(1)
      val expectedViolations = Violations(
        Seq(
          Violation.TooShortString("ab", 3),
          Violation.TooLongString("ab", 1),
        ),
      )

      assertZIO(validation.run("ab").either)(isLeft(equalTo(expectedViolations)))
    },
    test("result value when valid") {
      val validation = (Validations.minLength(1) |+| Validations.maxLength(2))
        .map(_.length)

      assertZIO(validation.run("ab"))(equalTo(2))
    },
  )

  val suiteForProduct = suite("forProduct()")(
    test("result violations when invalid") {
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

      val expectedViolations = Violations[Violation](
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
      )

      val result = validation.run(invalid)

      assertZIO(result.either)(isLeft(equalTo(expectedViolations)))
    },
    test("result transformed object when valid") {
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

      val expectedObject = Clean(
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
      )
      val result = validation.run(valid)

      assertZIO(result)(equalTo(expectedObject))
    },
  )

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("ZValidation")(
    suiteCombine,
    suiteForProduct,
  )

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
