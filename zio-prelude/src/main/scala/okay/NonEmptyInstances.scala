package okay

import okay.*
import okay.given
import zio.Chunk
import zio.NonEmptyChunk
import zio.ZIO
import zio.prelude.NonEmptyList
import zio.prelude.NonEmptySet

/** Given instance enabling `List[A].validateAs[NonEmptyList[A]]`. */
given listCanBeNonEmptyList[R, V, A](using
  Validation[R, V, Option[NonEmptyList[A]], NonEmptyList[A]],
): Validation[R, V, List[A], NonEmptyList[A]] =
  Validation.instance[List[A]] { list =>
    NonEmptyList.fromIterableOption(list).validateAs[NonEmptyList[A]]
  }

/** Given instance enabling `NonEmptyList[A].validateAs[NonEmptyList[B]]`. */
given nonEmptyListValidation[R, V, A, B](using
  v: Validation[R, V, A, B],
): Validation[R, V, NonEmptyList[A], NonEmptyList[B]] =
  Validation.instance[NonEmptyList[A]] { nel =>
    ZIO
      .foreach(nel.toList.zipWithIndex) { case (a, index) =>
        v.run(a).either
      }
      .map { results =>
        val (errors, successes) = results.partitionMap(identity)
        if (errors.isEmpty) Right(NonEmptyList.fromIterableOption(successes).get)
        else Left(errors.reduce(_ ++ _))
      }
      .absolve
  }

/** Given instance enabling `List[A].validateAs[NonEmptyList[B]]` with element validation. */
given listCanBeTransformedNonEmptyList[R, V, A, B](using
  Validation[R, V, Option[NonEmptyList[A]], NonEmptyList[A]],
  Validation[R, V, A, B],
): Validation[R, V, List[A], NonEmptyList[B]] = Validation.instance[List[A]] { list =>
  for {
    as <- list.validateAs[NonEmptyList[A]]
    bs <- as.validateAs[NonEmptyList[B]]
  } yield {
    bs
  }
}

/** Given instance enabling `Chunk[A].validateAs[NonEmptyChunk[A]]`. */
given chunkCanBeNonEmptyChunk[R, V, A](using
  v: Validation[R, V, Option[NonEmptyChunk[A]], NonEmptyChunk[A]],
): Validation[R, V, Chunk[A], NonEmptyChunk[A]] =
  Validation.instance[Chunk[A]] { chunk =>
    v.run(NonEmptyChunk.fromIterableOption(chunk))
  }

/** Given instance enabling `Set[A].validateAs[NonEmptySet[A]]`. */
given setCanBeNonEmptySet[R, V, A](using
  v: Validation[R, V, Option[NonEmptySet[A]], NonEmptySet[A]],
): Validation[R, V, Set[A], NonEmptySet[A]] =
  Validation.instance[Set[A]] { set =>
    v.run(NonEmptySet.fromIterableOption(set))
  }
