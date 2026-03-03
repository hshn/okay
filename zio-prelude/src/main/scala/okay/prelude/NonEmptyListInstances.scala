package okay.prelude

import okay.{*, given}
import zio.ZIO
import zio.prelude.{Validation as _, *}

given listCanBeNonEmptyList[R, V, A](using
  Validation[R, V, Option[NonEmptyList[A]], NonEmptyList[A]],
): Validation[R, V, List[A], NonEmptyList[A]] =
  Validation.instance[List[A]] { list =>
    NonEmptyList.fromIterableOption(list).validateAs[NonEmptyList[A]]
  }

given nonEmptyListValidation[R, V, A, B](using
  v: Validation[R, V, A, B],
): Validation[R, V, NonEmptyList[A], NonEmptyList[B]] =
  Validation.instance[NonEmptyList[A]] { nel =>
    nel.zipWithIndex.forEach1 { case (a, index) =>
      v.run(a).at(index)
    }
  }

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
