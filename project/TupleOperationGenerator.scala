object TupleOperationGenerator {
  def generateZValidatedSyntax: String = {
    val gens = (1 to 22)
      .map { n =>
        new TupleZValidatedGenerator(n)
      }

    s"""package okay.syntax
       |
       |import cats.implicits._
       |import okay._
       |import scala.language.implicitConversions
       |import zio._
       |
       |trait TupleZValidatedSyntax {
       |${gens.map(_.syntax).mkString("\n")}
       |}
       |
       |${gens.map(_.operation).mkString("\n")}
       |""".stripMargin
  }

  private class TupleZValidatedGenerator(n: Int) {
    val ns          = 1 to n
    val resultTypes = ns.map(n => s"B$n")
    val inputTypes  = resultTypes.map(t => s"ZIO[R, Violations[V], $t]")

    val operationClassName = s"Tuple${n}ZValidatedOps"

    val lookupValidationN: Int => String =
      if (n == 1) { _ => "validations" }
      else { n => s"validations._$n" }

    val tupleN: Int => String =
      if (n == 1) { _ => "" }
      else { n => s"._$n" }

    val mapN: String =
      if (n == 1) "map"
      else "mapN"

    def operation: String = {
      s"""final class $operationClassName[-R, +V, ${resultTypes.mkString(", ")}](
         |  private val validations: (
         |${inputTypes.map(t => s"    $t").mkString(",\n")}
         |  ),
         |) {
         |  def validateN[A](f: (${resultTypes.mkString(", ")}) => A): ZIO[R, Violations[V], A] = {
         |    (for {
         |${ns.map { n => s"      result$n <- validations${tupleN(n)}.either.map(_.toValidatedNec)" }.mkString("\n")}
         |    } yield (
         |${ns.map { n => s"      result$n" }.mkString(",\n")}
         |    ).$mapN(f).leftMap(_.combineAll).toEither).absolve
         |  }
         |}
         |""".stripMargin
    }

    def syntax: String = {
      s"""  implicit def syntaxZValidatedOps$n[R, V, ${resultTypes.mkString(", ")}](
         |    value: (
         |${inputTypes.map(t => s"      $t").mkString(",\n")}
         |    ),
         |  ): $operationClassName[R, V, ${resultTypes.mkString(", ")}] = new $operationClassName(value)
         |""".stripMargin
    }
  }

}
