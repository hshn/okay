package yoshi

import zio.prelude.{Validation as _, *}

private[yoshi] trait NonEmptySetInstances { self: NonEmptyListInstances =>

  implicit def setCanBeNonEmptySet[V, A](using
    Validation[V, Option[NonEmptySet[A]], NonEmptySet[A]],
  ): Validation[V, Set[A], NonEmptySet[A]] = Validation.instance[Set[A]] { as =>
    NonEmptySet.fromIterableOption(as).validateAs[NonEmptySet[A]]
  }

  implicit def nonEmptySetValidation[V, A, B](using
    v: Validation[V, A, B],
  ): Validation[V, NonEmptySet[A], NonEmptySet[B]] = Validation.instance[NonEmptySet[A]] { as =>
    for {
      bs <- as.toNonEmptyList.validateAs[NonEmptyList[B]]
    } yield {
      NonEmptySet.fromNonEmptyList(bs)
    }
  }

  implicit def setCanBeTransformedNonEmptySet[V, A, B](using
    Validation[V, Option[NonEmptySet[A]], NonEmptySet[A]],
    Validation[V, A, B],
  ): Validation[V, Set[A], NonEmptySet[B]] = Validation.instance[Set[A]] { set =>
    for {
      as <- set.validateAs[NonEmptySet[A]]
      bs <- as.validateAs[NonEmptySet[B]]
    } yield {
      bs
    }
  }
}
