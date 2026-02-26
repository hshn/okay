package okay

import cats.syntax.all.*
import okay.Validation.*
import okay.Violations.Path
import okay.defaults.{given, *}
import zio.Promise
import zio.Scope
import zio.ZIO
import zio.test.Assertion.*
import zio.test.Spec
import zio.test.TestEnvironment
import zio.test.ZIOSpecDefault
import zio.test.assertTrue
import zio.test.assertZIO

object ValidationSpec extends ZIOSpecDefault {
  given childValidation: Validation[Any, Violation, Dirty.Child, Clean.Child] =
    Validation.instance[Dirty.Child] { dirty =>
      (
        dirty.name.validateAs[String] at "name"
      ).validateN { name =>
        Clean.Child(name = name)
      }
    }

  val validation: Validation[Any, Violation, Dirty, Clean] = {
    Validation.instance[Dirty] { dirty =>
      (
        dirty.a1.validateAs[String].at("a1"),
        dirty.a2.validateAs[Int].at("a2"),
        dirty.a3.validateAs[List[Clean.Child]].at("a3"),
        dirty.a4.validateAs[Map[String, Clean.Child]].at("a4"),
      ).validateN { case (a1, a2, a3, a4) =>
        Clean(
          a1 = a1,
          a2 = a2,
          a3 = a3,
          a4 = a4,
        )
      }
    }
  }

  val suiteCombine = suite("|+|")(
    test("result violations when invalid") {
      val validation         = Validations.minLength(3) |+| Validations.maxLength(1)
      val expectedViolations = Violations(
        Vector(
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
    test("return left value when both succeed") {
      val left: Validation[Any, Violation, String, String]  = Validation.instance[String](s => ZIO.succeed(s.toUpperCase))
      val right: Validation[Any, Violation, String, String] = Validation.instance[String](s => ZIO.succeed(s.toLowerCase))

      assertZIO((left |+| right).run("Ab"))(equalTo("AB"))
    },
  )

  val suiteSequentialComposition = suite(">>")(
    test("chain two validations sequentially") {
      val first: Validation[Any, Violation, String, String] = Validations.minLength(1)
      val second: Validation[Any, Violation, String, Int]   = Validation.instance[String] { s =>
        ZIO.succeed(s.length)
      }

      assertZIO((first >> second).run("hello"))(equalTo(5))
    },
    test("fail on first validation") {
      val first: Validation[Any, Violation, String, String] = Validations.minLength(10)
      val second: Validation[Any, Violation, String, Int]   = Validation.instance[String] { s =>
        ZIO.succeed(s.length)
      }

      assertZIO((first >> second).run("hi").either)(
        isLeft(equalTo(Violations.single(Violation.TooShortString("hi", 10)))),
      )
    },
    test("fail on second validation") {
      val first: Validation[Any, Violation, String, String]  = Validations.minLength(1)
      val second: Validation[Any, Violation, String, String] = Validations.maxLength(2)

      assertZIO((first >> second).run("hello").either)(
        isLeft(equalTo(Violations.single(Violation.TooLongString("hello", 2)))),
      )
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
          Path("a1") -> Violations(Vector(Violation.Required)),
          Path("a2") -> Violations(Vector(Violation.NonIntegerString("yay"))),
          Path("a3") -> Violations(
            children = Map(
              Path(1) -> Violations(Vector(Violation.Required)).asChild("name"),
              Path(4) -> Violations(Vector(Violation.Required)).asChild("name"),
            ),
          ),
          Path("a4") -> Violations(
            children = Map(
              Path("a") -> Violations(Vector(Violation.Required)).asChild("name"),
              Path("c") -> Violations(Vector(Violation.Required)).asChild("name"),
              Path("d") -> Violations(Vector(Violation.Required)).asChild("name"),
              Path("f") -> Violations(Vector(Violation.Required)).asChild("name"),
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

  val suiteSucceed = suite("succeed")(
    test("returns the input unchanged") {
      assertZIO(Validation.succeed[String].run("hello"))(equalTo("hello"))
    },
    test("is right identity for >>") {
      val v = Validations.minLength(1)
      assertZIO((v >> Validation.succeed).run("ab"))(equalTo("ab"))
    },
    test("is left identity for >>") {
      val v = Validations.minLength(1)
      assertZIO((Validation.succeed >> v).run("ab"))(equalTo("ab"))
    },
  )

  val suiteFail = suite("fail")(
    test("always fails with the given violation") {
      val v = Validation.fail[Violation](Violation.Required)
      assertZIO(v.run("anything").either)(isLeft(equalTo(Violations.single[Violation](Violation.Required))))
    },
  )

  val suiteContramap = suite("contramap")(
    test("adapt input type before validation") {
      val nonEmpty: Validation[Any, Violation, String, String]            = Validations.minLength(1)
      val nameValidation: Validation[Any, Violation, Dirty.Child, String] = nonEmpty.contramap(_.name.getOrElse(""))

      assertZIO(nameValidation.run(Dirty.Child(name = "Alice".some)))(equalTo("Alice"))
    },
    test("propagate violations from adapted input") {
      val minLength3: Validation[Any, Violation, String, String]          = Validations.minLength(3)
      val nameValidation: Validation[Any, Violation, Dirty.Child, String] = minLength3.contramap(_.name.getOrElse(""))

      assertZIO(nameValidation.run(Dirty.Child(name = "ab".some)).either)(
        isLeft(equalTo(Violations(Vector(Violation.TooShortString("ab", 3))))),
      )
    },
  )

  val suiteOptional = suite("optional")(
    test("pass through None as success") {
      val v = Validations.minLength(3).optional
      assertZIO(v.run(None))(equalTo(None))
    },
    test("validate Some value and wrap result in Some") {
      val v = Validations.minLength(3).optional
      assertZIO(v.run(Some("hello")))(equalTo(Some("hello")))
    },
    test("fail when Some value is invalid") {
      val v = Validations.minLength(3).optional
      assertZIO(v.run(Some("ab")).either)(
        isLeft(equalTo(Violations.single(Violation.TooShortString("ab", 3)))),
      )
    },
    test("compose with map") {
      val v = Validations.minLength(1).optional.map(_.map(_.length))
      assertZIO(v.run(Some("hello")))(equalTo(Some(5)))
    },
    test("compose with sequential >>") {
      val v = (Validations.minLength(1) >> Validations.maxLength(5)).optional
      assertZIO(v.run(Some("abc")))(equalTo(Some("abc")))
    },
    test("fail in sequential >> composition") {
      val v = (Validations.minLength(1) >> Validations.maxLength(3)).optional
      assertZIO(v.run(Some("hello")).either)(
        isLeft(equalTo(Violations.single(Violation.TooLongString("hello", 3)))),
      )
    },
  )

  val suiteOptionCanBeValidatedAs = suite("optionCanBeValidatedAs")(
    test("derive Option validation from given") {
      given Validation[Any, Violation, String, String] = Validations.minLength(1)
      val v                                            = summon[Validation[Any, Violation, Option[String], Option[String]]]

      assertZIO(v.run(Some("hello")))(equalTo(Some("hello")))
    },
    test("pass through None") {
      given Validation[Any, Violation, String, String] = Validations.minLength(1)
      val v                                            = summon[Validation[Any, Violation, Option[String], Option[String]]]

      assertZIO(v.run(None))(equalTo(None))
    },
    test("fail when Some value is invalid") {
      given Validation[Any, Violation, String, String] = Validations.minLength(5)
      val v                                            = summon[Validation[Any, Violation, Option[String], Option[String]]]

      assertZIO(v.run(Some("ab")).either)(
        isLeft(equalTo(Violations.single(Violation.TooShortString("ab", 5)))),
      )
    },
  )

  val suiteSeqCanBeValidatedAs = suite("seqCanBeValidatedAs")(
    test("validate all elements in Seq") {
      given Validation[Any, Violation, String, String] = Validations.minLength(1)
      val v                                            = summon[Validation[Any, Violation, Seq[String], Seq[String]]]

      assertZIO(v.run(Seq("a", "bb", "ccc")))(equalTo(Seq("a", "bb", "ccc")))
    },
    test("succeed with empty Seq") {
      given Validation[Any, Violation, String, String] = Validations.minLength(1)
      val v                                            = summon[Validation[Any, Violation, Seq[String], Seq[String]]]

      assertZIO(v.run(Seq.empty))(equalTo(Seq.empty))
    },
    test("accumulate violations with indices") {
      given Validation[Any, Violation, String, String] = Validations.minLength(3)
      val v                                            = summon[Validation[Any, Violation, Seq[String], Seq[String]]]

      val expectedViolations = Violations[Violation](
        children = Map(
          Path(0) -> Violations(Vector(Violation.TooShortString("ab", 3))),
          Path(2) -> Violations(Vector(Violation.TooShortString("x", 3))),
        ),
      )

      assertZIO(v.run(Seq("ab", "hello", "x")).either)(
        isLeft(equalTo(expectedViolations)),
      )
    },
  )

  val suiteParallelism = suite("parallelism")(
    test("validateN runs validations in parallel") {
      for
        gate <- Promise.make[Nothing, Unit]
        v1 = Validation.instance[Unit](_ => gate.await.as("v1"))
        v2 = Validation.instance[Unit](_ => gate.succeed(()).as("v2"))
        result <- (v1.run(()), v2.run(())).validateN { case (a, b) => (a, b) }
      yield assertTrue(result == ("v1", "v2"))
    },
  )

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("ZValidation")(
    suiteCombine,
    suiteSequentialComposition,
    suiteSucceed,
    suiteFail,
    suiteContramap,
    suiteOptional,
    suiteOptionCanBeValidatedAs,
    suiteSeqCanBeValidatedAs,
    suiteForProduct,
    suiteParallelism,
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
