package okay.prelude

import okay.{*, given}
import zio.ZIO
import zio.prelude.{Validation as _, *}

given setCanBeNonEmptySet[R, V, A](using
  Validation[R, V, Option[NonEmptySet[A]], NonEmptySet[A]],
): Validation[R, V, Set[A], NonEmptySet[A]] = Validation.instance[Set[A]] { as =>
  NonEmptySet.fromIterableOption(as).validateAs[NonEmptySet[A]]
}

given nonEmptySetValidation[R, V, A, B](using
  v: Validation[R, V, A, B],
): Validation[R, V, NonEmptySet[A], NonEmptySet[B]] = Validation.instance[NonEmptySet[A]] { as =>
  for {
    bs <- as.toNonEmptyList.validateAs[NonEmptyList[B]]
  } yield {
    NonEmptySet.fromNonEmptyList(bs)
  }
}

given setCanBeTransformedNonEmptySet[R, V, A, B](using
  Validation[R, V, Option[NonEmptySet[A]], NonEmptySet[A]],
  Validation[R, V, A, B],
): Validation[R, V, Set[A], NonEmptySet[B]] = Validation.instance[Set[A]] { set =>
  for {
    as <- set.validateAs[NonEmptySet[A]]
    bs <- as.validateAs[NonEmptySet[B]]
  } yield {
    bs
  }
}
