package yoshi

import zio.prelude.Newtype

private[yoshi] trait NewtypeSyntax {

  extension (self: Validation.type)

    def newtype[V, A](nt: Newtype[A])(error: (A, String) => V): Validation[V, A, nt.Type] =
      Validation.instance[A] { a =>
        nt.make(a).toEither match {
          case Right(value) => Right(value)
          case Left(errors) => Left(Violations(errors.map(error(a, _)).toVector))
        }
      }
}
