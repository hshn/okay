package yoshi.syntax

import yoshi.Violations

/** Provides `.validateN(f)` for parallel validation of multiple `Either` values.
  *
  * {{{
  * (
  *   nameValidation.run(input.name).at("name"),
  *   ageValidation.run(input.age).at("age"),
  * ).validateN { case (name, age) => User(name, age) }
  * }}}
  */
trait ValidateN:
  extension [V, A](validation: Either[Violations[V], A])
    def validateN[B](f: A => B): Either[Violations[V], B] =
      validation.map(f)

  extension [V, T <: Tuple, Out <: Tuple](validations: T)(using tv: ValidateTuple[V, T, Out])
    def validateN[A](f: Out => A): Either[Violations[V], A] =
      tv.validate(validations).map(f)

sealed trait ValidateTuple[V, T <: Tuple, Out <: Tuple]:
  def validate(t: T): Either[Violations[V], Out]

object ValidateTuple:
  given empty[V]: ValidateTuple[V, EmptyTuple, EmptyTuple] with
    def validate(t: EmptyTuple): Either[Violations[V], EmptyTuple] =
      Right(EmptyTuple)

  given cons[V, H, T <: Tuple, TOut <: Tuple](using
    tail: ValidateTuple[V, T, TOut],
  ): ValidateTuple[V, Either[Violations[V], H] *: T, H *: TOut] with
    def validate(t: Either[Violations[V], H] *: T): Either[Violations[V], H *: TOut] =
      (t.head, tail.validate(t.tail)) match {
        case (Right(h), Right(t)) => Right(h *: t)
        case (Left(e1), Left(e2)) => Left(e1 ++ e2)
        case (Left(e), _)         => Left(e)
        case (_, Left(e))         => Left(e)
      }
