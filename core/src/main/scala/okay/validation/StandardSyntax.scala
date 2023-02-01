package okay.validation

import scala.language.implicitConversions

trait StandardSyntax {
  implicit def syntaxCanBeValidated[A](a: A): CanBeValidatedOps[A] = new CanBeValidatedOps[A](a)
}
