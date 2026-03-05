package yoshi

import yoshi.internal.fieldName
import yoshi.syntax.ValidatedAs
import zio.ZIO

/** A cursor over a value of type `A` that provides field access with automatic path tracking.
  *
  * Obtained via [[Validation.cursor]]. The cursor extracts field names from accessor lambdas at compile time, so violations automatically
  * carry the correct path without manual `.at("field")` calls.
  *
  * {{{
  * Validation.cursor[FormInput] { c =>
  *   (
  *     c.field(_.name).validateAs[String],              // path "name" derived from accessor
  *     c.field(_.age).validateAs[Int],                  // path "age" derived from accessor
  *     c.validateAs[String](_.name),                    // shorthand for field + validateAs
  *     c.field(_.name).at("alias").validateAs[String],  // path overridden to "alias"
  *   ).validateN { ... }
  * }
  * }}}
  *
  * @tparam A
  *   the input type to extract fields from
  * @see
  *   [[Validation.cursor]] for the factory method
  * @see
  *   [[CursorField]] for the result of `field()`
  */
final class ValidationCursor[A](private val underlying: A):

  /** Extract a field value and derive the path name from the accessor at compile time.
    *
    * For nested accessors like `_.address.zip`, only the last segment (`"zip"`) is used as the path.
    *
    * {{{
    * cursor.field(_.name) // CursorField("Alice", "name")
    * cursor.field(_.age)  // CursorField("30", "age")
    * }}}
    */
  inline def field[B](inline f: A => B): CursorField[B] =
    new CursorField(f(underlying), fieldName(f))

  /** Shorthand for `field(f).validateAs[B]` — extracts the field, validates it, and attaches the path in one step.
    *
    * {{{
    * // These are equivalent:
    * c.validateAs[String](_.name)
    * c.field(_.name).validateAs[String]
    * }}}
    */
  inline def validateAs[B] = new CursorValidateAs[A, B](underlying)

/** Intermediate class for the `cursor.validateAs[B](_.field)` shorthand.
  *
  * Not intended for direct use; obtained via [[ValidationCursor.validateAs]].
  */
final class CursorValidateAs[A, B](private val underlying: A):
  inline def apply[F](inline f: A => F)(using va: ValidatedAs[F, B]): ZIO[va.Env, Violations[va.Err], B] =
    f(underlying).validateAs[B].at(fieldName(f))

/** A field value paired with its auto-derived path, obtained via [[ValidationCursor.field]].
  *
  * Call [[validateAs]] to run validation with the path automatically attached, or [[at]] to override the path before validating.
  *
  * {{{
  * cursor.field(_.name).validateAs[String]                // path "name"
  * cursor.field(_.name).at("display_name").validateAs[String]  // path "display_name"
  * }}}
  *
  * @tparam B
  *   the field value type
  */
final class CursorField[B](val value: B, val path: String):

  /** Validate this field, automatically attaching the derived (or overridden) path to any violations. */
  def validateAs[C](using va: ValidatedAs[B, C]): ZIO[va.Env, Violations[va.Err], C] =
    value.validateAs[C].at(path)

  /** Override the auto-derived path.
    *
    * Returns a new [[CursorField]] with the same value but a different path. Useful when the domain field name differs from the input field
    * name.
    *
    * {{{
    * cursor.field(_.name).at("display_name").validateAs[String]
    * // violations will report path "display_name" instead of "name"
    * }}}
    */
  def at(customPath: String): CursorField[B] =
    new CursorField(value, customPath)
