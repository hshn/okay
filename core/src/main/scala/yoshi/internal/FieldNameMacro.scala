package yoshi.internal

import scala.quoted.*

/** Extract the field name from a simple accessor lambda at compile time.
  *
  * Supports only direct field access (e.g. `_.name`). Nested paths, method calls with arguments, or computed expressions will produce a
  * compile error.
  */
inline def fieldName[A, B](inline f: A => B): String =
  ${ fieldNameImpl[A, B]('f) }

private def fieldNameImpl[A: Type, B: Type](f: Expr[A => B])(using Quotes): Expr[String] =
  import quotes.reflect.*

  def extractName(term: Term): Option[String] = term match
    case Select(_, name)      => Some(name)
    case Inlined(_, _, inner) => extractName(inner)
    case Lambda(_, body)      => extractName(body)
    case Block(_, expr)       => extractName(expr)
    case Typed(expr, _)       => extractName(expr)
    case _                    => None

  extractName(f.asTerm) match
    case Some(name) => Expr(name)
    case None       =>
      report.errorAndAbort(
        s"Expected a simple field accessor like _.fieldName, but got: ${f.asTerm.show}",
      )
