package okay

import cats.kernel.Monoid

case class Violations[+V](
  values: Vector[V] = Vector.empty,
  children: Map[Violations.Path, Violations[V]] = Map.empty[Violations.Path, Violations[V]],
) {
  import Violations.*
  def asChild(path: Path): Violations[V]  = Violations[V](children = Map(path -> this))
  def asChild(key: String): Violations[V] = asChild(Path.Key(key))
  def asChild(index: Int): Violations[V]  = asChild(Path.Index(index))

  def toList: List[(Paths, V)] =
    values.iterator.map(v => (Paths.empty, v)).toList ++
      children.iterator.flatMap { case (path, child) =>
        child.toList.map { case (paths, v) => (Paths(path :: paths.segments), v) }
      }

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
  def single[V](value: V): Violations[V] = new Violations[V](Vector(value))

  private val _empty          = Violations[Nothing]()
  def empty[V]: Violations[V] = _empty

  given [V]: Monoid[Violations[V]] = Monoid.instance(empty, { _ ++ _ })

  enum Path:
    case Key(value: String)
    case Index(value: Int)

  object Path:
    def apply(value: String): Path = Path.Key(value)
    def apply(value: Int): Path    = Path.Index(value)

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
