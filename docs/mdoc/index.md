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

```scala mdoc:invisible
import yoshi.*
import yoshi.defaults.*
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

// Accumulating — collects all violations
val both = nonEmpty |+| maxLen

// Sequential — short-circuits on first failure
val chain = nonEmpty >> maxLen
```
