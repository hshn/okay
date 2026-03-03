package okay.prelude

import okay.*
import zio.ZIO
import zio.prelude.Newtype

extension (self: Validation.type)

  def newtype[V, A](nt: Newtype[A])(error: (A, String) => V): Validation[Any, V, A, nt.Type] =
    Validation.instance[A] { a =>
      nt.make(a).toEither match {
        case Right(value) => ZIO.succeed(value)
        case Left(errors) => ZIO.fail(Violations(errors.map(error(a, _)).toVector))
      }
    }
