package okay.syntax

import okay.Violations
import okay.Violations.Path
import zio.ZIO

final class ZValidatedOps[-R, +V, +A](private val self: ZIO[R, Violations[V], A]) extends AnyVal {
  def at(path: Path): ZIO[R, Violations[V], A] = self.mapError(_.asChild(path))
}
