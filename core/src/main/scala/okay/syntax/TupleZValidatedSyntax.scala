package okay.syntax

import cats.data.ValidatedNec
import cats.syntax.all.*
import okay.Violations
import zio.ZIO

trait TupleZValidatedSyntax:
  extension [R, V, A](validation: ZIO[R, Violations[V], A])
    def validateN[B](f: A => B): ZIO[R, Violations[V], B] =
      validation.map(f)

  extension [R, V, T <: Tuple, Out <: Tuple](validations: T)(using tv: ValidateTuple[R, V, T, Out])
    def validateN[A](f: Out => A): ZIO[R, Violations[V], A] =
      tv.validate(validations)
        .map(_.map(f).leftMap(_.combineAll).toEither)
        .absolve

sealed trait ValidateTuple[R, V, T <: Tuple, Out <: Tuple]:
  def validate(t: T): ZIO[R, Nothing, ValidatedNec[Violations[V], Out]]

object ValidateTuple:
  given empty[R, V]: ValidateTuple[R, V, EmptyTuple, EmptyTuple] with
    def validate(t: EmptyTuple): ZIO[R, Nothing, ValidatedNec[Violations[V], EmptyTuple]] =
      ZIO.succeed(EmptyTuple.validNec)

  given cons[R, V, H, T <: Tuple, TOut <: Tuple](using
    tail: ValidateTuple[R, V, T, TOut],
  ): ValidateTuple[R, V, ZIO[R, Violations[V], H] *: T, H *: TOut] with
    def validate(t: ZIO[R, Violations[V], H] *: T): ZIO[R, Nothing, ValidatedNec[Violations[V], H *: TOut]] =
      t.head.either.map(_.toValidatedNec)
        .zipPar(tail.validate(t.tail))
        .map { case (headResult, tailResult) => (headResult, tailResult).mapN(_ *: _) }
