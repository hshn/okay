package okay

import okay.Validation.*
import okay.Violations.Path
import okay.defaults.*
import zio.Promise
import zio.ZIO
import zio.test.*

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

  override def spec = suiteAll("ZValidation") {
    suiteAll("|+|") {
      test("result violations when invalid") {
        val validation         = Validations.minLength(3) |+| Validations.maxLength(1)
        val expectedViolations = Violations(
          Vector(
            Violation.TooShortString("ab", 3),
            Violation.TooLongString("ab", 1),
          ),
        )

        for result <- validation.run("ab").either
        yield assertTrue(result.is(_.left) == expectedViolations)
      }
      test("result value when valid") {
        val validation = (Validations.minLength(1) |+| Validations.maxLength(2))
          .map(_.length)

        for result <- validation.run("ab")
        yield assertTrue(result == 2)
      }
      test("return left value when both succeed") {
        val left: Validation[Any, Violation, String, String]  = Validation.instance[String](s => ZIO.succeed(s.toUpperCase))
        val right: Validation[Any, Violation, String, String] = Validation.instance[String](s => ZIO.succeed(s.toLowerCase))

        for result <- (left |+| right).run("Ab")
        yield assertTrue(result == "AB")
      }
    }
    suiteAll(">>") {
      test("chain two validations sequentially") {
        val first: Validation[Any, Violation, String, String] = Validations.minLength(1)
        val second: Validation[Any, Violation, String, Int]   = Validation.instance[String] { s =>
          ZIO.succeed(s.length)
        }

        for result <- (first >> second).run("hello")
        yield assertTrue(result == 5)
      }
      test("fail on first validation") {
        val first: Validation[Any, Violation, String, String] = Validations.minLength(10)
        val second: Validation[Any, Violation, String, Int]   = Validation.instance[String] { s =>
          ZIO.succeed(s.length)
        }

        for result <- (first >> second).run("hi").either
        yield assertTrue(result.is(_.left) == Violations.single(Violation.TooShortString("hi", 10)))
      }
      test("fail on second validation") {
        val first: Validation[Any, Violation, String, String]  = Validations.minLength(1)
        val second: Validation[Any, Violation, String, String] = Validations.maxLength(2)

        for result <- (first >> second).run("hello").either
        yield assertTrue(result.is(_.left) == Violations.single(Violation.TooLongString("hello", 2)))
      }
    }
    suiteAll("forProduct()") {
      test("result violations when invalid") {
        val invalid = Dirty(
          a1 = None,
          a2 = "yay",
          a3 = List(
            Dirty.Child(name = Some("0")),
            Dirty.Child(name = None),
            Dirty.Child(name = Some("2")),
            Dirty.Child(name = Some("3")),
            Dirty.Child(name = None),
            Dirty.Child(name = Some("5")),
          ),
          a4 = Map(
            "a" -> Dirty.Child(name = None),
            "b" -> Dirty.Child(name = Some("1")),
            "c" -> Dirty.Child(name = None),
            "d" -> Dirty.Child(name = None),
            "e" -> Dirty.Child(name = Some("4")),
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

        for result <- validation.run(invalid).either
        yield assertTrue(result.is(_.left) == expectedViolations)
      }
      test("result transformed object when valid") {
        val valid = Dirty(
          a1 = Some("hi"),
          a2 = "123",
          a3 = List(
            Dirty.Child(name = Some("0")),
            Dirty.Child(name = Some("2")),
            Dirty.Child(name = Some("3")),
            Dirty.Child(name = Some("5")),
          ),
          a4 = Map(
            "b" -> Dirty.Child(name = Some("1")),
            "e" -> Dirty.Child(name = Some("4")),
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

        for result <- validation.run(valid)
        yield assertTrue(result == expectedObject)
      }
    }
    suiteAll("succeed") {
      test("returns the input unchanged") {
        for result <- Validation.succeed[String].run("hello")
        yield assertTrue(result == "hello")
      }
      test("is right identity for >>") {
        val v = Validations.minLength(1)
        for result <- (v >> Validation.succeed).run("ab")
        yield assertTrue(result == "ab")
      }
      test("is left identity for >>") {
        val v = Validations.minLength(1)
        for result <- (Validation.succeed >> v).run("ab")
        yield assertTrue(result == "ab")
      }
    }
    suiteAll("fail") {
      test("always fails with the given violation") {
        val v = Validation.fail[Violation](Violation.Required)
        for result <- v.run("anything").either
        yield assertTrue(result.is(_.left) == Violations.single[Violation](Violation.Required))
      }
    }
    suiteAll("contramap") {
      test("adapt input type before validation") {
        val nonEmpty: Validation[Any, Violation, String, String]            = Validations.minLength(1)
        val nameValidation: Validation[Any, Violation, Dirty.Child, String] = nonEmpty.contramap(_.name.getOrElse(""))

        for result <- nameValidation.run(Dirty.Child(name = Some("Alice")))
        yield assertTrue(result == "Alice")
      }
      test("propagate violations from adapted input") {
        val minLength3: Validation[Any, Violation, String, String]          = Validations.minLength(3)
        val nameValidation: Validation[Any, Violation, Dirty.Child, String] = minLength3.contramap(_.name.getOrElse(""))

        for result <- nameValidation.run(Dirty.Child(name = Some("ab"))).either
        yield assertTrue(result.is(_.left) == Violations(Vector(Violation.TooShortString("ab", 3))))
      }
    }
    suiteAll("mapError") {
      test("transform error type on failure") {
        val v = Validations.minLength(3).mapError(_.toString)

        for result <- v.run("ab").either
        yield assertTrue(result.is(_.left) == Violations.single(Violation.TooShortString("ab", 3).toString))
      }
      test("preserve success value") {
        val v = Validations.minLength(1).mapError(_.toString)

        for result <- v.run("hello")
        yield assertTrue(result == "hello")
      }
      test("transform nested violations preserving structure") {
        val v = Validation
          .instance[String] { s =>
            ZIO.fail(
              Violations[Violation](
                values = Vector(Violation.Required),
                children = Map(
                  Path("child") -> Violations.single(Violation.TooShortString(s, 3)),
                ),
              ),
            )
          }
          .mapError(_.toString)

        for result <- v.run("ab").either
        yield assertTrue(
          result.is(_.left) == Violations[String](
            values = Vector(Violation.Required.toString),
            children = Map(
              Path("child") -> Violations.single(Violation.TooShortString("ab", 3).toString),
            ),
          ),
        )
      }
      test("compose with >>") {
        val first: Validation[Any, Violation, String, String]  = Validations.minLength(1)
        val second: Validation[Any, Violation, String, String] = Validations.maxLength(3)
        val v                                                  = (first >> second).mapError(_.toString)

        for result <- v.run("hello").either
        yield assertTrue(result.is(_.left) == Violations.single(Violation.TooLongString("hello", 3).toString))
      }
      test("compose with |+|") {
        val v = (Validations.minLength(3) |+| Validations.maxLength(1)).mapError(_.toString)

        for result <- v.run("ab").either
        yield assertTrue(
          result.is(_.left) == Violations(
            Vector(
              Violation.TooShortString("ab", 3).toString,
              Violation.TooLongString("ab", 1).toString,
            ),
          ),
        )
      }
    }
    suiteAll("optional") {
      test("pass through None as success") {
        val v = Validations.minLength(3).optional
        for result <- v.run(None)
        yield assertTrue(result == None)
      }
      test("validate Some value and wrap result in Some") {
        val v = Validations.minLength(3).optional
        for result <- v.run(Some("hello"))
        yield assertTrue(result == Some("hello"))
      }
      test("fail when Some value is invalid") {
        val v = Validations.minLength(3).optional
        for result <- v.run(Some("ab")).either
        yield assertTrue(result.is(_.left) == Violations.single(Violation.TooShortString("ab", 3)))
      }
      test("compose with map") {
        val v = Validations.minLength(1).optional.map(_.map(_.length))
        for result <- v.run(Some("hello"))
        yield assertTrue(result == Some(5))
      }
      test("compose with sequential >>") {
        val v = (Validations.minLength(1) >> Validations.maxLength(5)).optional
        for result <- v.run(Some("abc"))
        yield assertTrue(result == Some("abc"))
      }
      test("fail in sequential >> composition") {
        val v = (Validations.minLength(1) >> Validations.maxLength(3)).optional
        for result <- v.run(Some("hello")).either
        yield assertTrue(result.is(_.left) == Violations.single(Violation.TooLongString("hello", 3)))
      }
    }
    suiteAll("optionCanBeValidatedAs") {
      test("derive Option validation from given") {
        given Validation[Any, Violation, String, String] = Validations.minLength(1)
        val v                                            = summon[Validation[Any, Violation, Option[String], Option[String]]]

        for result <- v.run(Some("hello"))
        yield assertTrue(result == Some("hello"))
      }
      test("pass through None") {
        given Validation[Any, Violation, String, String] = Validations.minLength(1)
        val v                                            = summon[Validation[Any, Violation, Option[String], Option[String]]]

        for result <- v.run(None)
        yield assertTrue(result == None)
      }
      test("fail when Some value is invalid") {
        given Validation[Any, Violation, String, String] = Validations.minLength(5)
        val v                                            = summon[Validation[Any, Violation, Option[String], Option[String]]]

        for result <- v.run(Some("ab")).either
        yield assertTrue(result.is(_.left) == Violations.single(Violation.TooShortString("ab", 5)))
      }
    }
    suiteAll("seqCanBeValidatedAs") {
      test("validate all elements in Seq") {
        given Validation[Any, Violation, String, String] = Validations.minLength(1)
        val v                                            = summon[Validation[Any, Violation, Seq[String], Seq[String]]]

        for result <- v.run(Seq("a", "bb", "ccc"))
        yield assertTrue(result == Seq("a", "bb", "ccc"))
      }
      test("succeed with empty Seq") {
        given Validation[Any, Violation, String, String] = Validations.minLength(1)
        val v                                            = summon[Validation[Any, Violation, Seq[String], Seq[String]]]

        for result <- v.run(Seq.empty)
        yield assertTrue(result == Seq.empty)
      }
      test("accumulate violations with indices") {
        given Validation[Any, Violation, String, String] = Validations.minLength(3)
        val v                                            = summon[Validation[Any, Violation, Seq[String], Seq[String]]]

        val expectedViolations = Violations[Violation](
          children = Map(
            Path(0) -> Violations(Vector(Violation.TooShortString("ab", 3))),
            Path(2) -> Violations(Vector(Violation.TooShortString("x", 3))),
          ),
        )

        for result <- v.run(Seq("ab", "hello", "x")).either
        yield assertTrue(result.is(_.left) == expectedViolations)
      }
    }
    suiteAll("orElse") {
      test("return first result when first succeeds") {
        val first: Validation[Any, Violation, String, String]  = Validations.minLength(1)
        val second: Validation[Any, Violation, String, String] = Validations.minLength(5)

        for result <- first.orElse(second).run("ab")
        yield assertTrue(result == "ab")
      }
      test("fall back to second when first fails") {
        val first: Validation[Any, Violation, String, String]  = Validations.minLength(10)
        val second: Validation[Any, Violation, String, String] = Validations.minLength(1)

        for result <- first.orElse(second).run("hello")
        yield assertTrue(result == "hello")
      }
      test("return first violation when both fail") {
        val first: Validation[Any, Violation, String, String]  = Validations.minLength(10)
        val second: Validation[Any, Violation, String, String] = Validations.maxLength(1)

        for result <- first.orElse(second).run("hello").either
        yield assertTrue(result.is(_.left) == Violations.single(Violation.TooShortString("hello", 10)))
      }
      test("does not run second when first succeeds") {
        for
          called <- Promise.make[Nothing, Unit]
          first  = Validation.instance[String](s => ZIO.succeed(s))
          second = Validation.instance[String](_ => called.succeed(()) *> ZIO.succeed("fallback"))
          result    <- first.orElse(second).run("ok")
          wasCalled <- called.isDone
        yield assertTrue(result == "ok", !wasCalled)
      }
    }
    suiteAll("parallelism") {
      test("validateN runs validations in parallel") {
        for
          gate <- Promise.make[Nothing, Unit]
          v1 = Validation.instance[Unit](_ => gate.await.as("v1"))
          v2 = Validation.instance[Unit](_ => gate.succeed(()).as("v2"))
          result <- (v1.run(()), v2.run(())).validateN { case (a, b) => (a, b) }
        yield assertTrue(result == ("v1", "v2"))
      }
    }
  }

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
