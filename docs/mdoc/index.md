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
Yoshi gives you `Validation[R, V, A, B]` — a **composable function** from `A` to `B` that accumulates violations on failure.

```scala mdoc:invisible
import yoshi.*
import yoshi.defaults.*
import zio.*

def runUnsafe[V, A, B](v: ZIO[Any, Violations[V], B]): Either[Violations[V], B] =
  Unsafe.unsafe { implicit unsafe =>
    Runtime.default.unsafe.run(v.either).getOrThrow()
  }
```

```scala mdoc:silent
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
given Validation[Any, Violation, FormInput.Item, Order.Item] =
  Validation.cursor[FormInput.Item] { c =>
    (
      c.field(_.label).validateAs[String]
    ).validateN { label =>
      Order.Item(label)
    }
  }

val validation: Validation[Any, Violation, FormInput, Order] =
  Validation.cursor[FormInput] { c =>
    (
      c.field(_.name).validateAs[String],
      c.field(_.age).validateAs[Int],
      c.field(_.items).validateAs[List[Order.Item]],
    ).validateN { case (name, age, items) =>
      Order(name, age, items)
    }
  }
```

The cursor derives field names from accessor lambdas at compile time — no manual `.at("field")` strings needed.

For cases where the path should differ from the field name, use `.at()` to override:

```scala mdoc:silent
val renamed: Validation[Any, Violation, FormInput, Order] =
  Validation.cursor[FormInput] { c =>
    (
      c.field(_.name).at("display_name").validateAs[String],
      c.field(_.age).validateAs[Int],
      c.field(_.items).validateAs[List[Order.Item]],
    ).validateN { case (name, age, items) =>
      Order(name, age, items)
    }
  }
```

A shorthand is also available:

```scala mdoc:compile-only
// These are equivalent:
Validation.cursor[FormInput] { c =>
  c.field(_.name).validateAs[String]
  c.validateAs[String](_.name)
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
runUnsafe(validation.run(invalid)).left.map(_.toList)
```

## Automatic container derivation

Define a validator for `A → B`, and validators for `Option[A]`, `List[A]`, `Map[String, A]` are derived automatically:

```scala mdoc:compile-only
import yoshi.*
import yoshi.defaults.*

// Given this exists:
// given Validation[Any, Violation, String, Int]  (from yoshi.defaults)

// These are derived for free:
summon[Validation[Any, Violation, Option[String], Option[Int]]]
summon[Validation[Any, Violation, List[String], List[Int]]]
summon[Validation[Any, Violation, Map[String, String], Map[String, Int]]]
```

## Composing validators

```scala mdoc:silent
val nonEmpty: Validation[Any, String, String, String] =
  Validation.ensure("required")((_: String).nonEmpty)

val maxLen: Validation[Any, String, String, String] =
  Validation.maxLength(100)((_, _) => "too long")

// Parallel — accumulates all violations
val both = nonEmpty |+| maxLen

// Sequential — short-circuits on first failure
val chain = nonEmpty >> maxLen
```
