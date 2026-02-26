package okay

import cats.kernel.Monoid

/** A tree structure that accumulates validation violations with path information.
  *
  * Each node holds direct violations in `values` and nested violations in `children`, keyed by [[Violations.Path]] (field name or
  * collection index). This preserves the structural context of where each violation occurred.
  *
  * {{{
  * // Single violation at root
  * Violations.single("must not be empty")
  *
  * // Nested violation under a field path
  * Violations.single("too short").asChild("name")
  *
  * // Merge two violation trees
  * nameViolations ++ emailViolations
  *
  * // Flatten to a list of (path, violation) pairs
  * violations.toList  // List((Paths("name"), "too short"), ...)
  * }}}
  *
  * @tparam V
  *   the violation type
  * @param values
  *   direct violations at this level
  * @param children
  *   nested violations organized by path
  */
case class Violations[+V](
  values: Vector[V] = Vector.empty,
  children: Map[Violations.Path, Violations[V]] = Map.empty[Violations.Path, Violations[V]],
) {
  import Violations.*

  /** Transform every violation value in this tree using `f`. */
  def map[V1](f: V => V1): Violations[V1] =
    Violations[V1](
      values = values.map(f),
      children = children.map { case (path, child) => path -> child.map(f) },
    )

  /** Wrap this violations tree as a child under the given path. */
  def asChild(path: Path): Violations[V] = Violations[V](children = Map(path -> this))

  /** Wrap this violations tree as a child under a field key. */
  def asChild(key: String): Violations[V] = asChild(Path.Key(key))

  /** Wrap this violations tree as a child under a collection index. */
  def asChild(index: Int): Violations[V] = asChild(Path.Index(index))

  /** Flatten this tree into a list of `(path, violation)` pairs.
    *
    * {{{
    * val vs = Violations.single("err").asChild("field")
    * vs.toList  // List((Paths(List(Key("field"))), "err"))
    * }}}
    */
  def toList: List[(Paths, V)] =
    values.iterator.map(v => (Paths.empty, v)).toList ++
      children.iterator.flatMap { case (path, child) =>
        child.toList.map { case (paths, v) => (Paths(path :: paths.segments), v) }
      }

  /** Merge two violation trees, combining children with the same path recursively. */
  def ++[V1 >: V](other: Violations[V1]): Violations[V1] = {
    Violations[V1](
      values = values ++ other.values,
      children = other.children.foldLeft[Map[Path, Violations[V1]]](children) { case (acc, (ko, vo)) =>
        acc.updatedWith(ko) {
          case Some(v) => Some(v ++ vo)
          case None    => Some(vo)
        }
      },
    )
  }
}

object Violations {

  /** Create a [[Violations]] containing a single violation value. */
  def single[V](value: V): Violations[V] = new Violations[V](Vector(value))

  private val _empty = Violations[Nothing]()

  /** An empty [[Violations]] with no violations. */
  def empty[V]: Violations[V] = _empty

  given [V]: Monoid[Violations[V]] = Monoid.instance(empty, { _ ++ _ })

  /** A segment in a violation path, representing either a field key or a collection index. */
  enum Path:
    case Key(value: String)
    case Index(value: Int)

  object Path:
    def apply(value: String): Path = Path.Key(value)
    def apply(value: Int): Path    = Path.Index(value)

  /** A full path from the root to a violation, composed of [[Path]] segments.
    *
    * `toString` produces a human-readable dot-notation:
    * {{{
    * Paths(List(Key("address"), Key("zip"))).toString      // "address.zip"
    * Paths(List(Key("items"), Index(0), Key("name"))).toString  // "items[0].name"
    * }}}
    */
  case class Paths(segments: List[Path]):
    override def toString: String =
      segments
        .foldLeft(StringBuilder()) {
          case (sb, Path.Key(k)) =>
            if sb.nonEmpty then sb.append('.')
            sb.append(k)
          case (sb, Path.Index(i)) => sb.append('[').append(i).append(']')
        }
        .result()

  object Paths:
    val empty: Paths = Paths(Nil)
}
