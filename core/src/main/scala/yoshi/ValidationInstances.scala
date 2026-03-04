package yoshi

import zio.ZIO

trait ValidationInstances {

  /** Automatically lifts a `Validation[R, V, A, B]` to `Validation[R, V, Option[A], Option[B]]`. `None` passes through; `Some(a)` is
    * validated.
    */
  given optionCanBeValidatedAs[R, V, A, B](using validation: Validation[R, V, A, B]): Validation[R, V, Option[A], Option[B]] =
    validation.optional

  /** Automatically validates each element of a `Seq`, accumulating violations by index. */
  given seqCanBeValidatedAs[R, V, A, B](using validation: Validation[R, V, A, B]): Validation[R, V, Seq[A], Seq[B]] =
    Validation.instance { values =>
      ZIO
        .foreach(values.toList.zipWithIndex) { case (a, index) =>
          validation.run(a).at(index).either
        }
        .map { results =>
          val (errors, successes) = results.partitionMap(identity)
          if (errors.isEmpty) Right(successes)
          else Left(errors.reduce(_ ++ _))
        }
        .absolve
    }

  /** Automatically validates each element of a `List`, accumulating violations by index. */
  given listCanBeValidatedAs[R, V, A, B](using Validation[R, V, A, B]): Validation[R, V, List[A], List[B]] =
    seqCanBeValidatedAs[R, V, A, B].contramap[List[A]](identity).map(_.toList)

  /** Automatically validates each value of a `Map[String, A]`, accumulating violations by key. */
  given mapCanBeValidatedAs[R, V, A, B](using
    validation: Validation[R, V, A, B],
  ): Validation[R, V, Map[String, A], Map[String, B]] =
    Validation.instance { values =>
      ZIO
        .foreach(values.toList) { case (key, a) =>
          validation.run(a).at(key).map(b => key -> b).either
        }
        .map { results =>
          val (errors, successes) = results.partitionMap(identity)
          if (errors.isEmpty) Right(successes.toMap)
          else Left(errors.reduce(_ ++ _))
        }
        .absolve
    }
}
