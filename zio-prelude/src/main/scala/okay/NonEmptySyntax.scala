package okay

import zio.{Chunk, NonEmptyChunk, ZIO}
import zio.prelude.{NonEmptyList, NonEmptySet}

extension (self: Validation.type)

  /** Validate that a `List` is non-empty with an explicit error, returning a [[zio.prelude.NonEmptyList]]. */
  def nonEmptyList[V, A](error: => V): Validation[Any, V, List[A], NonEmptyList[A]] =
    Validation.instance[List[A]] { list =>
      NonEmptyList.fromIterableOption(list) match {
        case Some(nel) => ZIO.succeed(nel)
        case None      => ZIO.fail(Violations.single(error))
      }
    }

  /** Validate that a `Chunk` is non-empty with an explicit error, returning a [[zio.NonEmptyChunk]]. */
  def nonEmptyChunk[V, A](error: => V): Validation[Any, V, Chunk[A], NonEmptyChunk[A]] =
    Validation.instance[Chunk[A]] { chunk =>
      NonEmptyChunk.fromIterableOption(chunk) match {
        case Some(nec) => ZIO.succeed(nec)
        case None      => ZIO.fail(Violations.single(error))
      }
    }

  /** Validate that a `Set` is non-empty with an explicit error, returning a [[zio.prelude.NonEmptySet]]. */
  def nonEmptySet[V, A](error: => V): Validation[Any, V, Set[A], NonEmptySet[A]] =
    Validation.instance[Set[A]] { set =>
      NonEmptySet.fromIterableOption(set) match {
        case Some(nes) => ZIO.succeed(nes)
        case None      => ZIO.fail(Violations.single(error))
      }
    }
