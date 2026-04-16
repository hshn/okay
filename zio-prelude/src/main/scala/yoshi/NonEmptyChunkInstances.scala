package yoshi

import zio.Chunk
import zio.NonEmptyChunk
import zio.prelude.{Validation as _, *}

private[yoshi] trait NonEmptyChunkInstances { self: AssociativeBothInstances =>

  implicit def chunkCanBeNonEmptyChunk[V, A](using
    Validation[V, Option[NonEmptyChunk[A]], NonEmptyChunk[A]],
  ): Validation[V, Chunk[A], NonEmptyChunk[A]] =
    Validation.instance[Chunk[A]] { as =>
      NonEmptyChunk.fromIterableOption(as).validateAs[NonEmptyChunk[A]]
    }

  implicit def nonEmptyChunkValidation[V, A, B](using
    v: Validation[V, A, B],
  ): Validation[V, NonEmptyChunk[A], NonEmptyChunk[B]] =
    Validation.instance[NonEmptyChunk[A]] { nec =>
      nec.zipWithIndex.forEach1 { case (a, index) =>
        v.run(a).at(index)
      }
    }

  implicit def chunkCanBeTransformedNonEmptyChunk[V, A, B](using
    Validation[V, Option[NonEmptyChunk[A]], NonEmptyChunk[A]],
    Validation[V, A, B],
  ): Validation[V, Chunk[A], NonEmptyChunk[B]] = Validation.instance[Chunk[A]] { chunk =>
    for {
      as <- chunk.validateAs[NonEmptyChunk[A]]
      bs <- as.validateAs[NonEmptyChunk[B]]
    } yield {
      bs
    }
  }
}
