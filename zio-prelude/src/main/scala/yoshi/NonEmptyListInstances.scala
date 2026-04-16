package yoshi

import zio.prelude.{Validation as _, *}

private[yoshi] trait NonEmptyListInstances { self: AssociativeBothInstances =>

  implicit def listCanBeNonEmptyList[V, A](using
    Validation[V, Option[NonEmptyList[A]], NonEmptyList[A]],
  ): Validation[V, List[A], NonEmptyList[A]] =
    Validation.instance[List[A]] { list =>
      NonEmptyList.fromIterableOption(list).validateAs[NonEmptyList[A]]
    }

  implicit def nonEmptyListValidation[V, A, B](using
    v: Validation[V, A, B],
  ): Validation[V, NonEmptyList[A], NonEmptyList[B]] =
    Validation.instance[NonEmptyList[A]] { nel =>
      nel.zipWithIndex.forEach1 { case (a, index) =>
        v.run(a).at(index)
      }
    }

  implicit def listCanBeTransformedNonEmptyList[V, A, B](using
    Validation[V, Option[NonEmptyList[A]], NonEmptyList[A]],
    Validation[V, A, B],
  ): Validation[V, List[A], NonEmptyList[B]] = Validation.instance[List[A]] { list =>
    for {
      as <- list.validateAs[NonEmptyList[A]]
      bs <- as.validateAs[NonEmptyList[B]]
    } yield {
      bs
    }
  }
}
