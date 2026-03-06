package yoshi.internal

import scala.quoted.*

/** Extract field names from an accessor lambda at compile time.
  *
  * {{{
  * fieldNames(_.name)         // List("name")
  * fieldNames(_.address.zip)  // List("address", "zip")
  * }}}
  *
  * Method calls with arguments or computed expressions will produce a compile error.
  */
inline def fieldNames[A, B](inline f: A => B): List[String] =
  ${ fieldNamesImpl[A, B]('f) }

private def fieldNamesImpl[A: Type, B: Type](f: Expr[A => B])(using Quotes): Expr[List[String]] =
  import quotes.reflect.*

  def collectNames(term: Term): Option[List[String]] = term match
    case Ident(_)                => Some(Nil)
    case Select(qualifier, name) => collectNames(qualifier).map(_ :+ name)
    case Inlined(_, _, inner)    => collectNames(inner)
    case Lambda(_, body)         => collectNames(body)
    case Block(_, expr)          => collectNames(expr)
    case Typed(expr, _)          => collectNames(expr)
    case _                       => None

  collectNames(f.asTerm) match
    case Some(names) if names.nonEmpty => Expr(names)
    case _                             =>
      report.errorAndAbort(
        s"Expected a field accessor like _.fieldName, but got: ${f.asTerm.show}",
      )
