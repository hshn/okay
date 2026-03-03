package okay.syntax

import okay.Violations
import zio.ZIO

/** Provides `.validateN(f)` for parallel validation of multiple ZIO effects.
  *
  * {{{
  * (
  *   nameValidation.run(input.name).at("name"),
  *   ageValidation.run(input.age).at("age"),
  * ).validateN { case (name, age) => User(name, age) }
  * }}}
  */
trait ValidateN:
  extension [R, V, A](validation: ZIO[R, Violations[V], A])
    def validateN[B](f: A => B): ZIO[R, Violations[V], B] =
      validation.map(f)

  extension [R, V, T <: Tuple, Out <: Tuple](validations: T)(using tv: ValidateTuple[R, V, T, Out])
    def validateN[A](f: Out => A): ZIO[R, Violations[V], A] =
      tv.validate(validations)
        .map(_.map(f))
        .absolve

sealed trait ValidateTuple[R, V, T <: Tuple, Out <: Tuple]:
  def validate(t: T): ZIO[R, Nothing, Either[Violations[V], Out]]

object ValidateTuple:
  given empty[R, V]: ValidateTuple[R, V, EmptyTuple, EmptyTuple] with
    def validate(t: EmptyTuple): ZIO[R, Nothing, Either[Violations[V], EmptyTuple]] =
      ZIO.succeed(Right(EmptyTuple))

  given cons[R, V, H, T <: Tuple, TOut <: Tuple](using
    tail: ValidateTuple[R, V, T, TOut],
  ): ValidateTuple[R, V, ZIO[R, Violations[V], H] *: T, H *: TOut] with
    def validate(t: ZIO[R, Violations[V], H] *: T): ZIO[R, Nothing, Either[Violations[V], H *: TOut]] =
      t.head.either
        .zipPar(tail.validate(t.tail))
        .map {
          case (Right(h), Right(t)) => Right(h *: t)
          case (Left(e1), Left(e2)) => Left(e1 ++ e2)
          case (Left(e), _)         => Left(e)
          case (_, Left(e))         => Left(e)
        }
