# yoshi

A validation library that transforms untyped input into domain types — not just checking values, but parsing them into a stronger representation.

## The idea

Most validation libraries give you `Validated[E, A]` — a value that's either valid or not.
Yoshi gives you `Validation[R, V, A, B]` — a **composable function** from `A` to `B` that accumulates violations on failure.

```scala
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

```scala
import yoshi.*
import yoshi.defaults.*

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

```scala
c.field(_.name).at("display_name").validateAs[String]
```

A shorthand is also available:

```scala
// These are equivalent:
c.field(_.name).validateAs[String]
c.validateAs[String](_.name)
```

<details>
<summary>Without cursor (manual <code>.at()</code> calls)</summary>

```scala
val validation: Validation[Any, Violation, FormInput, Order] =
  Validation.instance[FormInput] { dirty =>
    (
      dirty.name.validateAs[String].at("name"),
      dirty.age.validateAs[Int].at("age"),
      dirty.items.validateAs[List[Order.Item]].at("items"),
    ).validateN { case (name, age, items) =>
      Order(name, age, items)
    }
  }
```

</details>

## Path-aware error accumulation

Violations carry the full path to where they occurred:

```scala
validation.run(invalid).either.map {
  case Left(violations) =>
    violations.toList.foreach { case (path, v) => println(s"$path: $v") }
    // name: Required
    // age: NonIntegerString(abc)
    // items[1].label: Required
}
```

## Automatic container derivation

Define a validator for `A → B`, and validators for `Option[A]`, `List[A]`, `Map[String, A]` are derived automatically:

```scala
// Given this exists:
given Validation[Any, Violation, String, String] = ...

// These are derived for free:
// Validation[Any, Violation, Option[String], Option[String]]
// Validation[Any, Violation, List[String], List[String]]
// Validation[Any, Violation, Map[String, String], Map[String, String]]
```

## Composing validators

```scala
val nonEmpty = Validation.ensure("required")((_: String).nonEmpty)
val maxLen   = Validation.maxLength(100)(_ => "too long")

// Parallel — accumulates all violations
val both = nonEmpty |+| maxLen

// Sequential — short-circuits on first failure
val chain = nonEmpty >> maxLen
```
