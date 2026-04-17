package yoshi

import zio.prelude.AssociativeBoth

private[yoshi] trait AssociativeBothInstances {

  implicit def violationsAssociativeBoth[V]: AssociativeBoth[[X] =>> Either[Violations[V], X]] =
    new AssociativeBoth[[X] =>> Either[Violations[V], X]] {
      def both[A, B](fa: => Either[Violations[V], A], fb: => Either[Violations[V], B]): Either[Violations[V], (A, B)] =
        (fa, fb) match {
          case (Right(a), Right(b)) => Right((a, b))
          case (Left(e1), Left(e2)) => Left(e1 ++ e2)
          case (Left(e), _)         => Left(e)
          case (_, Left(e))         => Left(e)
        }
    }
}
