package okay

import cats.kernel.Monoid

case class Violations[+V](
  values: Seq[V] = Nil,
  children: Map[Violations.Path, Violations[V]] = Map.empty[Violations.Path, Violations[V]],
) {
  import Violations._
  def asChild(path: Path): Violations[V]   = Violations[V](children = Map(path -> this))
  def asChild(key: String): Violations[V]  = asChild(Path.Key(key))
  def asChild(index: Int): Violations[V]   = asChild(Path.Index(index))

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
  def single[V](value: V): Violations[V] = new Violations[V](value :: Nil)

  private val _empty          = Violations[Nothing]()
  def empty[V]: Violations[V] = _empty

  given [V]: Monoid[Violations[V]] = Monoid.instance(empty, { _ ++ _ })

  sealed trait Path extends Product with Serializable
  object Path {
    def apply(value: String): Path = Key(value)
    def apply(value: Int): Path    = Index(value)
    case class Key(value: String) extends Path
    case class Index(value: Int)  extends Path

  }
}
