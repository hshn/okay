package okay.prelude

import okay.*
import zio.ZIO
import zio.prelude.AssociativeBoth

given violationsAssociativeBoth[R, V]: AssociativeBoth[[X] =>> ZIO[R, Violations[V], X]] with
  def both[A, B](fa: => ZIO[R, Violations[V], A], fb: => ZIO[R, Violations[V], B]): ZIO[R, Violations[V], (A, B)] =
    (fa.either zip fb.either).flatMap {
      case (Right(a), Right(b)) => ZIO.succeed((a, b))
      case (Left(e1), Left(e2)) => ZIO.fail(e1 ++ e2)
      case (Left(e), _)         => ZIO.fail(e)
      case (_, Left(e))         => ZIO.fail(e)
    }
