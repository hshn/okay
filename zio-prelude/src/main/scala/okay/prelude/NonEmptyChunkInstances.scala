package okay.prelude

import okay.{*, given}
import zio.Chunk
import zio.NonEmptyChunk
import zio.ZIO
import zio.prelude.{Validation as _, *}

given chunkCanBeNonEmptyChunk[R, V, A](using
  Validation[R, V, Option[NonEmptyChunk[A]], NonEmptyChunk[A]],
): Validation[R, V, Chunk[A], NonEmptyChunk[A]] =
  Validation.instance[Chunk[A]] { as =>
    NonEmptyChunk.fromIterableOption(as).validateAs[NonEmptyChunk[A]]
  }

given nonEmptyChunkValidation[R, V, A, B](using
  v: Validation[R, V, A, B],
): Validation[R, V, NonEmptyChunk[A], NonEmptyChunk[B]] =
  Validation.instance[NonEmptyChunk[A]] { nec =>
    nec.zipWithIndex.forEach1 { case (a, index) =>
      v.run(a).at(index)
    }
  }

given chunkCanBeTransformedNonEmptyChunk[R, V, A, B](using
  Validation[R, V, Option[NonEmptyChunk[A]], NonEmptyChunk[A]],
  Validation[R, V, A, B],
): Validation[R, V, Chunk[A], NonEmptyChunk[B]] = Validation.instance[Chunk[A]] { chunk =>
  for {
    as <- chunk.validateAs[NonEmptyChunk[A]]
    bs <- as.validateAs[NonEmptyChunk[B]]
  } yield {
    bs
  }
}
