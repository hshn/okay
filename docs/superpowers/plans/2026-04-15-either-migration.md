# Either Migration Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Migrate `Validation[-R, +V, -A, +B]` to `Validation[+V, -A, +B]` with `Either[Violations[V], B]` result type, removing ZIO from core compile scope.

**Architecture:** Replace all ZIO effect usage in core with pure `Either`. The `R` type parameter is removed from the entire public API. zio-prelude module is temporarily excluded from the build (out of scope for this migration).

**Tech Stack:** Scala 3.3, sbt, ZIO Test (test scope only)

**Note:** Tasks 2–7 modify interdependent source files. Core will not compile until all of Tasks 2–7 are complete. Run `sbt core/compile` after Task 7 to verify.

---

### Task 1: Update build.sbt

**Files:**
- Modify: `build.sbt`

- [ ] **Step 1: Remove compile-scope ZIO and exclude zio-prelude**

```scala
lazy val root = (project in file(".") withId "yoshi")
  .settings(
    Compile / unmanagedSourceDirectories   := Nil,
    Compile / unmanagedResourceDirectories := Nil,
    Test / unmanagedSourceDirectories      := Nil,
    Test / unmanagedResourceDirectories    := Nil,
  )
  .aggregate(
    core,
  )

lazy val core = (project in file("core"))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-test"          % zio % Test,
      "dev.zio" %% "zio-test-sbt"      % zio % Test,
      "dev.zio" %% "zio-test-magnolia" % zio % Test,
    ),
  )
```

Changes:
- Remove `zio-prelude` from `.aggregate(...)` (temporarily — it won't compile against the new core)
- Remove `"dev.zio" %% "zio"` from core's `libraryDependencies` (only test deps remain)

- [ ] **Step 2: Commit**

```
git add build.sbt
git commit -m "build: remove compile-scope ZIO from core, exclude zio-prelude"
```

---

### Task 2: Migrate Validation.scala

**Files:**
- Modify: `core/src/main/scala/yoshi/Validation.scala`

- [ ] **Step 1: Replace the full file content**

Remove `import zio.ZIO`. Change sealed class and all methods:

```scala
package yoshi

import scala.util.matching.Regex

sealed abstract class Validation[+V, -A, +B] { self =>

  def run(a: A): Either[Violations[V], B]

  def mapError[V1](f: V => V1): Validation[V1, A, B] =
    Validation.instance[A] { a =>
      self.run(a).left.map(_.map(f))
    }

  def contramap[A0](f: A0 => A): Validation[V, A0, B] =
    Validation.instance[A0] { a0 =>
      self.run(f(a0))
    }

  def map[C](f: B => C): Validation[V, A, C] =
    Validation.instance[A] { a =>
      for {
        b <- run(a)
      } yield {
        f(b)
      }
    }

  def |+|[V1 >: V, A1 <: A, B1 >: B](next: Validation[V1, A1, B1]): Validation[V1, A1, B1] =
    Validation.instance { a =>
      val lhs: Either[Violations[V1], B1] = run(a)
      val rhs: Either[Violations[V1], B1] = next.run(a)
      (lhs, rhs).validateN { case (b1, _) => b1 }
    }

  def >>[C, V1 >: V](next: Validation[V1, B, C]): Validation[V1, A, C] =
    Validation.instance[A] { a =>
      for {
        b <- run(a)
        c <- next.run(b)
      } yield {
        c
      }
    }

  def orElse[V1 >: V, A1 <: A, B1 >: B](that: Validation[V1, A1, B1]): Validation[V1, A1, B1] =
    Validation.instance[A1] { a =>
      self.run(a) match
        case r @ Right(_) => r
        case Left(firstError) =>
          that.run(a) match
            case r @ Right(_) => r
            case Left(_)      => Left(firstError)
    }

  def optional: Validation[V, Option[A], Option[B]] =
    Validation.instance[Option[A]] {
      case Some(a) => self.run(a).map(Some(_))
      case None    => Right(None)
    }
}

object Validation extends ValidationInstances {

  final private class Impl[+V, -A, +B](f: A => Either[Violations[V], B]) extends Validation[V, A, B] {
    def run(a: A): Either[Violations[V], B] = f(a)
  }

  def instance[A] = new InstancePartiallyApplied[A]

  final class InstancePartiallyApplied[A] {
    def apply[V, B](f: A => Either[Violations[V], B]): Validation[V, A, B] = new Impl(f)
  }

  def cursor[A] = new CursorPartiallyApplied[A]

  final class CursorPartiallyApplied[A] {
    def apply[V, B](f: ValidationCursor[A] => Either[Violations[V], B]): Validation[V, A, B] =
      new Impl(a => f(new ValidationCursor(a)))
  }

  def succeed[A]: Validation[Nothing, A, A] =
    instance[A](a => Right(a))

  def fail[V](v: V): Validation[V, Any, Nothing] =
    instance[Any](_ => Left(Violations.of(v)))

  def ensure[V, A](f: => V)(test: A => Boolean): Validation[V, A, A] =
    ensureOr[V, A](_ => f)(test)

  def ensureOr[V, A](f: A => V)(test: A => Boolean): Validation[V, A, A] = instance[A] { a =>
    if (test(a))
      Right(a)
    else
      Left(Violations.of(f(a)))
  }

  def required[V, A](error: => V): Validation[V, Option[A], A] =
    instance[Option[A]](_.toRight(Violations.of(error)))

  def parseInt[V](error: String => V): Validation[V, String, Int] =
    instance[String] { value =>
      try Right(Integer.parseInt(value))
      catch {
        case _: NumberFormatException =>
          Left(Violations.of(error(value)))
      }
    }

  def maxLength[V](max: Int)(error: (String, Int) => V): Validation[V, String, String] =
    ensureOr[V, String] { value =>
      error(value, max)
    } { value =>
      value.length <= max
    }

  def minLength[V](min: Int)(error: (String, Int) => V): Validation[V, String, String] =
    ensureOr[V, String] { value =>
      error(value, min)
    } { value =>
      value.length >= min
    }

  def min[V](n: Int)(error: Int => V): Validation[V, Int, Int] =
    ensureOr[V, Int](error)(_ >= n)

  def max[V](n: Int)(error: Int => V): Validation[V, Int, Int] =
    ensureOr[V, Int](error)(_ <= n)

  def positive[V](error: Int => V): Validation[V, Int, Int] =
    ensureOr[V, Int](error)(_ > 0)

  def matches[V](pattern: Regex)(error: (String, Regex) => V): Validation[V, String, String] =
    instance[String] { value =>
      if (pattern.matches(value)) Right(value)
      else Left(Violations.of(error(value, pattern)))
    }

}
```

- [ ] **Step 2: Commit**

```
git add core/src/main/scala/yoshi/Validation.scala
git commit -m "refactor: migrate Validation core type from ZIO to Either"
```

---

### Task 3: Migrate ValidateAs.scala

**Files:**
- Modify: `core/src/main/scala/yoshi/syntax/ValidateAs.scala`

- [ ] **Step 1: Replace the full file content**

```scala
package yoshi.syntax

import yoshi.Validation
import yoshi.Violations

trait ValidateAs:
  extension [A](a: A) def validateAs[B](using va: ValidatedAs[A, B]): Either[Violations[va.Err], B] = va.run(a)

  extension [V, A](value: Either[Violations[V], A])
    def at(path: Violations.Path): Either[Violations[V], A] = value.left.map(_.asChild(path))
    def at(key: String): Either[Violations[V], A]           = at(Violations.Path.Key(key))
    def at(index: Int): Either[Violations[V], A]            = at(Violations.Path.Index(index))

sealed trait ValidatedAs[-A, +B]:
  type Err
  def run(a: A): Either[Violations[Err], B]

object ValidatedAs:
  final class Derived[V, A, B](v: Validation[V, A, B]) extends ValidatedAs[A, B]:
    type Err = V
    def run(a: A): Either[Violations[V], B] = v.run(a)

  transparent inline given derive[V, A, B](using v: Validation[V, A, B]): ValidatedAs[A, B] =
    new Derived(v)
```

- [ ] **Step 2: Commit**

```
git add core/src/main/scala/yoshi/syntax/ValidateAs.scala
git commit -m "refactor: migrate ValidateAs/ValidatedAs from ZIO to Either"
```

---

### Task 4: Migrate ValidateN.scala

**Files:**
- Modify: `core/src/main/scala/yoshi/syntax/ValidateN.scala`

- [ ] **Step 1: Replace the full file content**

```scala
package yoshi.syntax

import yoshi.Violations

trait ValidateN:
  extension [V, A](validation: Either[Violations[V], A])
    def validateN[B](f: A => B): Either[Violations[V], B] =
      validation.map(f)

  extension [V, T <: Tuple, Out <: Tuple](validations: T)(using tv: ValidateTuple[V, T, Out])
    def validateN[A](f: Out => A): Either[Violations[V], A] =
      tv.validate(validations).map(f)

sealed trait ValidateTuple[V, T <: Tuple, Out <: Tuple]:
  def validate(t: T): Either[Violations[V], Out]

object ValidateTuple:
  given empty[V]: ValidateTuple[V, EmptyTuple, EmptyTuple] with
    def validate(t: EmptyTuple): Either[Violations[V], EmptyTuple] =
      Right(EmptyTuple)

  given cons[V, H, T <: Tuple, TOut <: Tuple](using
    tail: ValidateTuple[V, T, TOut],
  ): ValidateTuple[V, Either[Violations[V], H] *: T, H *: TOut] with
    def validate(t: Either[Violations[V], H] *: T): Either[Violations[V], H *: TOut] =
      (t.head, tail.validate(t.tail)) match
        case (Right(h), Right(t)) => Right(h *: t)
        case (Left(e1), Left(e2)) => Left(e1 ++ e2)
        case (Left(e), _)         => Left(e)
        case (_, Left(e))         => Left(e)
```

- [ ] **Step 2: Commit**

```
git add core/src/main/scala/yoshi/syntax/ValidateN.scala
git commit -m "refactor: migrate ValidateN from ZIO to Either"
```

---

### Task 5: Migrate Cursor.scala

**Files:**
- Modify: `core/src/main/scala/yoshi/Cursor.scala`

- [ ] **Step 1: Replace the full file content**

```scala
package yoshi

import yoshi.Violations.Path
import yoshi.Violations.Paths
import yoshi.internal.fieldNames
import yoshi.syntax.ValidatedAs

final class ValidationCursor[A](private val underlying: A):

  inline def field[B](inline f: A => B): CursorField[B] =
    new CursorField(f(underlying), Paths(fieldNames(f).map(Path.Key(_))))

  inline def validateAs[B] = new CursorValidateAs[A, B](underlying)

final class CursorValidateAs[A, B](private val underlying: A):
  inline def apply[F](inline f: A => F)(using va: ValidatedAs[F, B]): Either[Violations[va.Err], B] =
    val paths = Paths(fieldNames(f).map(Path.Key(_)))
    va.run(f(underlying)).left.map(_.asChild(paths))

final class CursorField[B](val value: B, val path: Paths):

  def validateAs[C](using va: ValidatedAs[B, C]): Either[Violations[va.Err], C] =
    va.run(value).left.map(_.asChild(path))

  def at(customPath: String): CursorField[B] =
    new CursorField(value, Paths(List(Path.Key(customPath))))
```

- [ ] **Step 2: Commit**

```
git add core/src/main/scala/yoshi/Cursor.scala
git commit -m "refactor: migrate Cursor from ZIO to Either"
```

---

### Task 6: Migrate ValidationInstances.scala

**Files:**
- Modify: `core/src/main/scala/yoshi/ValidationInstances.scala`

- [ ] **Step 1: Replace the full file content**

```scala
package yoshi

trait ValidationInstances {

  given optionCanBeValidatedAs[V, A, B](using validation: Validation[V, A, B]): Validation[V, Option[A], Option[B]] =
    validation.optional

  given seqCanBeValidatedAs[V, A, B](using validation: Validation[V, A, B]): Validation[V, Seq[A], Seq[B]] =
    Validation.instance { values =>
      val results = values.toList.zipWithIndex.map { case (a, index) =>
        validation.run(a).left.map(_.asChild(index))
      }
      val (errors, successes) = results.partitionMap(identity)
      if (errors.isEmpty) Right(successes)
      else Left(errors.reduce(_ ++ _))
    }

  given listCanBeValidatedAs[V, A, B](using Validation[V, A, B]): Validation[V, List[A], List[B]] =
    seqCanBeValidatedAs[V, A, B].contramap[List[A]](identity).map(_.toList)

  given mapCanBeValidatedAs[V, A, B](using
    validation: Validation[V, A, B],
  ): Validation[V, Map[String, A], Map[String, B]] =
    Validation.instance { values =>
      val results = values.toList.map { case (key, a) =>
        validation.run(a).left.map(_.asChild(key)).map(b => key -> b)
      }
      val (errors, successes) = results.partitionMap(identity)
      if (errors.isEmpty) Right(successes.toMap)
      else Left(errors.reduce(_ ++ _))
    }
}
```

- [ ] **Step 2: Commit**

```
git add core/src/main/scala/yoshi/ValidationInstances.scala
git commit -m "refactor: migrate ValidationInstances from ZIO to Either"
```

---

### Task 7: Migrate defaults

**Files:**
- Modify: `core/src/main/scala/yoshi/defaults/ValidationInstances.scala`
- Modify: `core/src/main/scala/yoshi/defaults/Validations.scala`

- [ ] **Step 1: Update defaults/ValidationInstances.scala**

```scala
package yoshi.defaults

import yoshi.Validation

implicit def optionCanBeDefined[A]: Validation[Violation, Option[A], A] =
  Validations.required

implicit val stringCanBeInt: Validation[Violation, String, Int] =
  Validations.parseInt
```

- [ ] **Step 2: Update defaults/Validations.scala**

```scala
package yoshi.defaults

import scala.util.matching.Regex
import yoshi.*

object Validations {

  def required[A]: Validation[Violation, Option[A], A] =
    Validation.required(Violation.Required)

  def parseInt: Validation[Violation, String, Int] =
    Validation.parseInt(Violation.NonIntegerString(_))

  def maxLength(max: Int): Validation[Violation, String, String] =
    Validation.maxLength(max)(Violation.TooLongString(_, _))

  def minLength(min: Int): Validation[Violation, String, String] =
    Validation.minLength(min)(Violation.TooShortString(_, _))

  def matches(pattern: Regex): Validation[Violation, String, String] =
    Validation.matches(pattern)(Violation.Unmatched(_, _))

  def min(n: Int): Validation[Violation, Int, Int] =
    Validation.min(n)(Violation.TooSmall(_, n))

  def max(n: Int): Validation[Violation, Int, Int] =
    Validation.max(n)(Violation.TooLarge(_, n))

  def positive: Validation[Violation, Int, Int] =
    Validation.positive(Violation.NonPositive(_))

}
```

- [ ] **Step 3: Verify core compiles**

Run: `sbt core/compile`
Expected: Compilation succeeds

- [ ] **Step 4: Commit**

```
git add core/src/main/scala/yoshi/defaults/ValidationInstances.scala core/src/main/scala/yoshi/defaults/Validations.scala
git commit -m "refactor: update defaults type parameters for Either migration"
```

---

### Task 8: Migrate tests — ValidationTransformsSpec

**Files:**
- Modify: `core/src/test/scala/yoshi/ValidationTransformsSpec.scala`

- [ ] **Step 1: Replace the full file content**

Key changes:
- Remove `import zio.Promise` and `import zio.ZIO`
- `Validation[Any, V, A, B]` → `Validation[V, A, B]`
- `for result <- v.run(x) yield assertTrue(...)` → `assertTrue(v.run(x) == Right(...))`
- `for result <- v.run(x).either yield assertTrue(...)` → `assertTrue(v.run(x) == Left(...))`
- `Validation.instance[String](s => ZIO.succeed(...))` → `Validation.instance[String](s => Right(...))`
- `ZIO.fail(...)` → `Left(...)`
- Promise-based "does not run" test → use `var` to track side effects

```scala
package yoshi

import yoshi.defaults.*
import zio.test.*

object ValidationTransformsSpec extends ZIOSpecDefault {
  override def spec = suiteAll("Validation transforms") {
    suiteAll("succeed") {
      test("returns the input unchanged") {
        assertTrue(Validation.succeed[String].run("hello") == Right("hello"))
      }
      test("is right identity for >>") {
        val v = Validations.minLength(1)
        assertTrue((v >> Validation.succeed).run("ab") == Right("ab"))
      }
      test("is left identity for >>") {
        val v = Validations.minLength(1)
        assertTrue((Validation.succeed >> v).run("ab") == Right("ab"))
      }
    }
    suiteAll("fail") {
      test("always fails with the given violation") {
        val v = Validation.fail[Violation](Violation.Required)
        assertTrue(v.run("anything") == Left(Violations.of[Violation](Violation.Required)))
      }
    }
    suiteAll("contramap") {
      test("adapt input type before validation") {
        val nonEmpty: Validation[Violation, String, String]          = Validations.minLength(1)
        val nameValidation: Validation[Violation, NameInput, String] =
          nonEmpty.contramap(_.name.getOrElse(""))

        assertTrue(nameValidation.run(NameInput(name = Some("Alice"))) == Right("Alice"))
      }
      test("propagate violations from adapted input") {
        val minLength3: Validation[Violation, String, String]        = Validations.minLength(3)
        val nameValidation: Validation[Violation, NameInput, String] =
          minLength3.contramap(_.name.getOrElse(""))

        assertTrue(nameValidation.run(NameInput(name = Some("ab"))) == Left(Violations(Vector(Violation.TooShortString("ab", 3)))))
      }
    }
    suiteAll("mapError") {
      test("transform error type on failure") {
        val v = Validations.minLength(3).mapError(_.toString)

        assertTrue(v.run("ab") == Left(Violations.of(Violation.TooShortString("ab", 3).toString)))
      }
      test("preserve success value") {
        val v = Validations.minLength(1).mapError(_.toString)

        assertTrue(v.run("hello") == Right("hello"))
      }
      test("transform nested violations preserving structure") {
        val v = Validation
          .instance[String] { s =>
            Left(
              Violations[Violation](
                values = Vector(Violation.Required),
                children = Map(
                  Violations.Path("child") -> Violations.of(Violation.TooShortString(s, 3)),
                ),
              ),
            )
          }
          .mapError(_.toString)

        assertTrue(
          v.run("ab") == Left(
            Violations[String](
              values = Vector(Violation.Required.toString),
              children = Map(
                Violations.Path("child") -> Violations.of(Violation.TooShortString("ab", 3).toString),
              ),
            ),
          ),
        )
      }
      test("compose with >>") {
        val first: Validation[Violation, String, String]  = Validations.minLength(1)
        val second: Validation[Violation, String, String] = Validations.maxLength(3)
        val v                                             = (first >> second).mapError(_.toString)

        assertTrue(v.run("hello") == Left(Violations.of(Violation.TooLongString("hello", 3).toString)))
      }
      test("compose with |+|") {
        val v = (Validations.minLength(3) |+| Validations.maxLength(1)).mapError(_.toString)

        assertTrue(
          v.run("ab") == Left(
            Violations(
              Vector(
                Violation.TooShortString("ab", 3).toString,
                Violation.TooLongString("ab", 1).toString,
              ),
            ),
          ),
        )
      }
    }
    suiteAll("optional") {
      test("pass through None as success") {
        val v = Validations.minLength(3).optional
        assertTrue(v.run(None) == Right(None))
      }
      test("validate Some value and wrap result in Some") {
        val v = Validations.minLength(3).optional
        assertTrue(v.run(Some("hello")) == Right(Some("hello")))
      }
      test("fail when Some value is invalid") {
        val v = Validations.minLength(3).optional
        assertTrue(v.run(Some("ab")) == Left(Violations.of(Violation.TooShortString("ab", 3))))
      }
      test("compose with map") {
        val v = Validations.minLength(1).optional.map(_.map(_.length))
        assertTrue(v.run(Some("hello")) == Right(Some(5)))
      }
      test("compose with sequential >>") {
        val v = (Validations.minLength(1) >> Validations.maxLength(5)).optional
        assertTrue(v.run(Some("abc")) == Right(Some("abc")))
      }
      test("fail in sequential >> composition") {
        val v = (Validations.minLength(1) >> Validations.maxLength(3)).optional
        assertTrue(v.run(Some("hello")) == Left(Violations.of(Violation.TooLongString("hello", 3))))
      }
      test("does not run underlying validation for None") {
        var called = false
        val raw    = Validation.instance[String] { _ => called = true; Right("ok") }
        raw.optional.run(None)
        assertTrue(!called)
      }
    }
  }

  private case class NameInput(name: Option[String])
}
```

- [ ] **Step 2: Commit**

```
git add core/src/test/scala/yoshi/ValidationTransformsSpec.scala
git commit -m "test: migrate ValidationTransformsSpec to Either"
```

---

### Task 9: Migrate tests — ValidationCombinatorsSpec

**Files:**
- Modify: `core/src/test/scala/yoshi/ValidationCombinatorsSpec.scala`

- [ ] **Step 1: Replace the full file content**

Key changes:
- Remove `import zio.Promise` and `import zio.ZIO`
- `Validation.instance[String](s => ZIO.succeed(...))` → `Validation.instance[String](s => Right(...))`
- Promise-based "does not run" tests → use `var`

```scala
package yoshi

import yoshi.Validation.*
import yoshi.defaults.*
import zio.test.*

object ValidationCombinatorsSpec extends ZIOSpecDefault {
  override def spec = suiteAll("Validation combinators") {
    suiteAll("|+|") {
      test("result violations when invalid") {
        val validation         = Validations.minLength(3) |+| Validations.maxLength(1)
        val expectedViolations = Violations(
          Vector(
            Violation.TooShortString("ab", 3),
            Violation.TooLongString("ab", 1),
          ),
        )

        assertTrue(validation.run("ab") == Left(expectedViolations))
      }
      test("result value when valid") {
        val validation = (Validations.minLength(1) |+| Validations.maxLength(2))
          .map(_.length)

        assertTrue(validation.run("ab") == Right(2))
      }
      test("return left value when both succeed") {
        val left: Validation[Violation, String, String] =
          Validation.instance[String](s => Right(s.toUpperCase))
        val right: Validation[Violation, String, String] =
          Validation.instance[String](s => Right(s.toLowerCase))

        assertTrue((left |+| right).run("Ab") == Right("AB"))
      }
    }
    suiteAll(">>") {
      test("chain two validations sequentially") {
        val first: Validation[Violation, String, String] = Validations.minLength(1)
        val second: Validation[Violation, String, Int] = Validation.instance[String] { s =>
          Right(s.length)
        }

        assertTrue((first >> second).run("hello") == Right(5))
      }
      test("fail on first validation") {
        val first: Validation[Violation, String, String] = Validations.minLength(10)
        val second: Validation[Violation, String, Int] = Validation.instance[String] { s =>
          Right(s.length)
        }

        assertTrue((first >> second).run("hi") == Left(Violations.of(Violation.TooShortString("hi", 10))))
      }
      test("fail on second validation") {
        val first: Validation[Violation, String, String]  = Validations.minLength(1)
        val second: Validation[Violation, String, String] = Validations.maxLength(2)

        assertTrue((first >> second).run("hello") == Left(Violations.of(Violation.TooLongString("hello", 2))))
      }
      test("does not run second validation when first fails") {
        var secondCalled = false
        val first        = Validations.minLength(10)
        val second       = Validation.instance[String] { _ => secondCalled = true; Right(0) }
        (first >> second).run("hi")
        assertTrue(!secondCalled)
      }
    }
    suiteAll("orElse") {
      test("return first result when first succeeds") {
        val first: Validation[Violation, String, String]  = Validations.minLength(1)
        val second: Validation[Violation, String, String] = Validations.minLength(5)

        assertTrue(first.orElse(second).run("ab") == Right("ab"))
      }
      test("fall back to second when first fails") {
        val first: Validation[Violation, String, String]  = Validations.minLength(10)
        val second: Validation[Violation, String, String] = Validations.minLength(1)

        assertTrue(first.orElse(second).run("hello") == Right("hello"))
      }
      test("return first violation when both fail") {
        val first: Validation[Violation, String, String]  = Validations.minLength(10)
        val second: Validation[Violation, String, String] = Validations.maxLength(1)

        assertTrue(first.orElse(second).run("hello") == Left(Violations.of(Violation.TooShortString("hello", 10))))
      }
      test("does not run second when first succeeds") {
        var secondCalled = false
        val first        = Validation.instance[String](s => Right(s))
        val second       = Validation.instance[String] { _ => secondCalled = true; Right("fallback") }
        first.orElse(second).run("ok")
        assertTrue(!secondCalled)
      }
    }
  }
}
```

- [ ] **Step 2: Commit**

```
git add core/src/test/scala/yoshi/ValidationCombinatorsSpec.scala
git commit -m "test: migrate ValidationCombinatorsSpec to Either"
```

---

### Task 10: Migrate tests — ValidationProductSpec

**Files:**
- Modify: `core/src/test/scala/yoshi/ValidationProductSpec.scala`

- [ ] **Step 1: Replace the full file content**

Key changes:
- Remove `import zio.Promise`
- Remove the "validateN runs validations in parallel" test (no parallelism with Either)
- `Validation[Any, V, A, B]` → `Validation[V, A, B]`

```scala
package yoshi

import yoshi.Validation.*
import yoshi.Violations.Path
import yoshi.defaults.*
import zio.test.*

object ValidationProductSpec extends ZIOSpecDefault {
  given childValidation: Validation[Violation, Dirty.Child, Clean.Child] =
    Validation.instance[Dirty.Child] { dirty =>
      (
        dirty.name.validateAs[String].at("name")
      ).validateN { name =>
        Clean.Child(name = name)
      }
    }

  val validation: Validation[Violation, Dirty, Clean] =
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

  override def spec = suiteAll("Validation product") {
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

        assertTrue(validation.run(invalid) == Left(expectedViolations))
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

        assertTrue(validation.run(valid) == Right(expectedObject))
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
```

- [ ] **Step 2: Commit**

```
git add core/src/test/scala/yoshi/ValidationProductSpec.scala
git commit -m "test: migrate ValidationProductSpec to Either"
```

---

### Task 11: Migrate tests — ValidationsSpec, CursorSpec, ValidationTypeclassSpec

**Files:**
- Modify: `core/src/test/scala/yoshi/ValidationsSpec.scala`
- Modify: `core/src/test/scala/yoshi/CursorSpec.scala`
- Modify: `core/src/test/scala/yoshi/ValidationTypeclassSpec.scala`

These tests have no ZIO-specific logic (no `Promise`, no `ZIO.succeed`/`ZIO.fail`). Changes are mechanical:
- `for result <- v.run(x) yield assertTrue(...)` → `assertTrue(v.run(x) == Right(...))`
- `for result <- v.run(x).either yield assertTrue(result.is(_.left) == ...)` → `assertTrue(v.run(x) == Left(...))`
- `Validation[Any, V, A, B]` → `Validation[V, A, B]`
- Multi-validation `for` blocks → sequential `val` bindings

- [ ] **Step 1: Update ValidationsSpec.scala**

```scala
package yoshi

import yoshi.defaults.*
import zio.test.*

object ValidationsSpec extends ZIOSpecDefault {
  override def spec = suiteAll("Validations") {
    suiteAll("required() can extract from Option") {
      test("success") {
        assertTrue(Validations.required[String].run(Some("hello")) == Right("hello"))
      }
      test("failure") {
        assertTrue(Validations.required[String].run(None) == Left(Violations.of(Violation.Required)))
      }
    }
    suiteAll("parseInt() can parse string to int") {
      test("success") {
        assertTrue(Validations.parseInt.run("42") == Right(42))
      }
      test("failure") {
        assertTrue(Validations.parseInt.run("abc") == Left(Violations.of(Violation.NonIntegerString("abc"))))
      }
    }
    suiteAll("maxLength() can test string length") {
      test("success") {
        val value = "a".repeat(4)
        assertTrue(Validations.maxLength(max = 4).run(value) == Right(value))
      }
      test("failure") {
        val value = "a".repeat(5)
        assertTrue(Validations.maxLength(max = 4).run(value) == Left(Violations.of(Violation.TooLongString(value, maxLength = 4))))
      }
    }
    suiteAll("minLength() can test string length") {
      test("success") {
        val value = "a".repeat(4)
        assertTrue(Validations.minLength(min = 4).run(value) == Right(value))
      }
      test("failure") {
        val value = "a".repeat(3)
        assertTrue(Validations.minLength(min = 4).run(value) == Left(Violations.of(Violation.TooShortString(value, minLength = 4))))
      }
    }
    suiteAll("min() can test minimum value") {
      test("success") {
        assertTrue(Validations.min(n = 5).run(5) == Right(5))
      }
      test("failure") {
        assertTrue(Validations.min(n = 5).run(4) == Left(Violations.of(Violation.TooSmall(value = 4, min = 5))))
      }
    }
    suiteAll("max() can test maximum value") {
      test("success") {
        assertTrue(Validations.max(n = 10).run(10) == Right(10))
      }
      test("failure") {
        assertTrue(Validations.max(n = 10).run(11) == Left(Violations.of(Violation.TooLarge(value = 11, max = 10))))
      }
    }
    suiteAll("positive() can test positive value") {
      test("success") {
        assertTrue(Validations.positive.run(1) == Right(1))
      }
      test("failure with zero") {
        assertTrue(Validations.positive.run(0) == Left(Violations.of(Violation.NonPositive(value = 0))))
      }
      test("failure with negative") {
        assertTrue(Validations.positive.run(-1) == Left(Violations.of(Violation.NonPositive(value = -1))))
      }
    }
    suiteAll("matches() can test with regex") {
      test("success") {
        val pattern = "^abc$".r
        val value   = "abc"
        assertTrue(Validations.matches(pattern).run(value) == Right(value))
      }
      test("failure") {
        val pattern = "^abc$".r
        val value   = "abcd"
        assertTrue(Validations.matches(pattern).run(value) == Left(Violations.of(Violation.Unmatched(value, pattern))))
      }
    }
  }
}
```

- [ ] **Step 2: Update CursorSpec.scala**

```scala
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

        assertTrue(validation.run(invalid) == manualValidation.run(invalid))
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

        assertTrue(withField.run(invalid) == withShorthand.run(invalid))
      }
    }
  }
}
```

- [ ] **Step 3: Update ValidationTypeclassSpec.scala**

```scala
package yoshi

import yoshi.Violations.Path
import yoshi.defaults.*
import zio.test.*

object ValidationTypeclassSpec extends ZIOSpecDefault {
  override def spec = suiteAll("Validation typeclass instances") {
    suiteAll("optionCanBeValidatedAs") {
      test("derive Option validation from given") {
        given Validation[Violation, String, String] = Validations.minLength(1)
        val v                                       = summon[Validation[Violation, Option[String], Option[String]]]

        assertTrue(v.run(Some("hello")) == Right(Some("hello")))
      }
      test("pass through None") {
        given Validation[Violation, String, String] = Validations.minLength(1)
        val v                                       = summon[Validation[Violation, Option[String], Option[String]]]

        assertTrue(v.run(None) == Right(None))
      }
      test("fail when Some value is invalid") {
        given Validation[Violation, String, String] = Validations.minLength(5)
        val v                                       = summon[Validation[Violation, Option[String], Option[String]]]

        assertTrue(v.run(Some("ab")) == Left(Violations.of(Violation.TooShortString("ab", 5))))
      }
    }
    suiteAll("seqCanBeValidatedAs") {
      test("validate all elements in Seq") {
        given Validation[Violation, String, String] = Validations.minLength(1)
        val v                                       = summon[Validation[Violation, Seq[String], Seq[String]]]

        assertTrue(v.run(Seq("a", "bb", "ccc")) == Right(Seq("a", "bb", "ccc")))
      }
      test("succeed with empty Seq") {
        given Validation[Violation, String, String] = Validations.minLength(1)
        val v                                       = summon[Validation[Violation, Seq[String], Seq[String]]]

        assertTrue(v.run(Seq.empty) == Right(Seq.empty))
      }
      test("accumulate violations with indices") {
        given Validation[Violation, String, String] = Validations.minLength(3)
        val v                                       = summon[Validation[Violation, Seq[String], Seq[String]]]

        val expectedViolations = Violations[Violation](
          children = Map(
            Path(0) -> Violations(Vector(Violation.TooShortString("ab", 3))),
            Path(2) -> Violations(Vector(Violation.TooShortString("x", 3))),
          ),
        )

        assertTrue(v.run(Seq("ab", "hello", "x")) == Left(expectedViolations))
      }
    }
    suiteAll("listCanBeValidatedAs") {
      test("validate all elements in List") {
        given Validation[Violation, String, String] = Validations.minLength(1)
        val v                                       = summon[Validation[Violation, List[String], List[String]]]

        assertTrue(v.run(List("a", "bb", "ccc")) == Right(List("a", "bb", "ccc")))
      }
      test("accumulate violations with indices") {
        given Validation[Violation, String, String] = Validations.minLength(3)
        val v                                       = summon[Validation[Violation, List[String], List[String]]]

        val expectedViolations = Violations[Violation](
          children = Map(
            Path(0) -> Violations(Vector(Violation.TooShortString("ab", 3))),
            Path(2) -> Violations(Vector(Violation.TooShortString("x", 3))),
          ),
        )

        assertTrue(v.run(List("ab", "hello", "x")) == Left(expectedViolations))
      }
    }
    suiteAll("mapCanBeValidatedAs") {
      test("validate all values in Map") {
        given Validation[Violation, String, String] = Validations.minLength(1)
        val v                                       = summon[Validation[Violation, Map[String, String], Map[String, String]]]
        val input                                   = Map(
          "a" -> "x",
          "b" -> "yy",
        )

        assertTrue(v.run(input) == Right(input))
      }
      test("accumulate violations with keys") {
        given Validation[Violation, String, String] = Validations.minLength(3)
        val v                                       = summon[Validation[Violation, Map[String, String], Map[String, String]]]

        val expectedViolations = Violations[Violation](
          children = Map(
            Path("a") -> Violations(Vector(Violation.TooShortString("ab", 3))),
            Path("c") -> Violations(Vector(Violation.TooShortString("x", 3))),
          ),
        )

        assertTrue(v.run(Map("a" -> "ab", "b" -> "hello", "c" -> "x")) == Left(expectedViolations))
      }
    }
  }
}
```

- [ ] **Step 4: Commit**

```
git add core/src/test/scala/yoshi/ValidationsSpec.scala core/src/test/scala/yoshi/CursorSpec.scala core/src/test/scala/yoshi/ValidationTypeclassSpec.scala
git commit -m "test: migrate ValidationsSpec, CursorSpec, ValidationTypeclassSpec to Either"
```

---

### Task 12: Run tests and verify

- [ ] **Step 1: Run all core tests**

Run: `sbt core/test`
Expected: All tests pass

- [ ] **Step 2: Fix any compilation or test failures**

If failures occur, diagnose and fix. Common issues:
- Missing import removals
- Type parameter count mismatches
- `.either` calls that were not removed

- [ ] **Step 3: Commit any fixes**

```
git add -p
git commit -m "fix: resolve test failures from Either migration"
```

---

### Task 13: Update documentation

**Files:**
- Modify: `docs/mdoc/index.md`

- [ ] **Step 1: Replace the full file content**

```markdown
---
id: index
title: Getting Started
slug: /
sidebar_position: 1
---

# yoshi

A validation library that transforms untyped input into domain types — not just checking values, but parsing them into a stronger representation.

## Setup

```scala
libraryDependencies += "io.github.hshn" %% "yoshi-core" % "@VERSION@"
```

## The idea

Most validation libraries give you `Validated[E, A]` — a value that's either valid or not.
Yoshi gives you `Validation[V, A, B]` — a **composable function** from `A` to `B` that accumulates violations on failure.

```scala mdoc:silent
import yoshi.*
import yoshi.defaults.*

// Untyped input — what you receive
case class FormInput(
  name: Option[String],
  age: String,
  items: List[FormInput.Item],
)
object FormInput:
  case class Item(label: Option[String])

// Domain type — what you want
case class Order(
  name: String,
  age: Int,
  items: List[Order.Item],
)
object Order:
  case class Item(label: String)
```

```scala mdoc:silent
given Validation[Violation, FormInput.Item, Order.Item] =
  Validation.cursor[FormInput.Item] { c =>
    (
      c.validateAs[String](_.label)
    ).validateN { label =>
      Order.Item(label)
    }
  }

val validation: Validation[Violation, FormInput, Order] =
  Validation.cursor[FormInput] { c =>
    (
      c.validateAs[String](_.name),
      c.validateAs[Int](_.age),
      c.validateAs[List[Order.Item]](_.items),
    ).validateN { case (name, age, items) =>
      Order(name, age, items)
    }
  }
```

The cursor derives field names from accessor lambdas at compile time — no manual `.at("field")` strings needed.

When the path should differ from the field name, use `field()` with `.at()` to override:

```scala mdoc:silent
val renamed: Validation[Violation, FormInput, Order] =
  Validation.cursor[FormInput] { c =>
    (
      c.field(_.name).at("display_name").validateAs[String],
      c.validateAs[Int](_.age),
      c.validateAs[List[Order.Item]](_.items),
    ).validateN { case (name, age, items) =>
      Order(name, age, items)
    }
  }
```

## Path-aware error accumulation

Violations carry the full path to where they occurred:

```scala mdoc:silent
val invalid = FormInput(
  name = None,
  age = "abc",
  items = List(
    FormInput.Item(Some("pen")),
    FormInput.Item(None),
  ),
)
```

```scala mdoc
validation.run(invalid).left.map(_.toList)
```

## Automatic container derivation

Define a validator for `A → B`, and validators for `Option[A]`, `List[A]`, `Map[String, A]` are derived automatically:

```scala mdoc:compile-only
import yoshi.*
import yoshi.defaults.*

// Given this exists:
// given Validation[Violation, String, Int]  (from yoshi.defaults)

// These are derived for free:
summon[Validation[Violation, Option[String], Option[Int]]]
summon[Validation[Violation, List[String], List[Int]]]
summon[Validation[Violation, Map[String, String], Map[String, Int]]]
```

## Composing validators

```scala mdoc:silent
val nonEmpty: Validation[String, String, String] =
  Validation.ensure("required")((_: String).nonEmpty)

val maxLen: Validation[String, String, String] =
  Validation.maxLength(100)((_, _) => "too long")

// Parallel — accumulates all violations
val both = nonEmpty |+| maxLen

// Sequential — short-circuits on first failure
val chain = nonEmpty >> maxLen
```
```

- [ ] **Step 2: Commit**

```
git add docs/mdoc/index.md
git commit -m "docs: update examples for Either-based validation API"
```

---

### Task 14: Final verification

- [ ] **Step 1: Run all core tests again**

Run: `sbt core/test`
Expected: All tests pass

- [ ] **Step 2: Verify no ZIO imports remain in core main sources**

Run: `grep -r "import zio" core/src/main/`
Expected: No output (no ZIO imports in core main sources)
