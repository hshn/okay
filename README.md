# yoshi

[![CI](https://img.shields.io/github/actions/workflow/status/hshn/yoshi/ci.yml?branch=master&label=CI)](https://github.com/hshn/yoshi/actions/workflows/ci.yml)
[![Maven Central](https://img.shields.io/maven-central/v/dev.hshn/yoshi_3?label=Maven%20Central)](https://central.sonatype.com/artifact/dev.hshn/yoshi_3)

A validation library for Scala 3 that transforms untyped input into domain types — not just checking values, but parsing them into a stronger representation.

```scala
import yoshi.*
import yoshi.defaults.*

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

Full documentation is available at [hshn.github.io/yoshi](https://hshn.github.io/yoshi/).
