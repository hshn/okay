package okay

import cats.kernel.Monoid

case class Violations[V](
  values: Seq[V] = Nil,
  children: Map[Violations.Path, Violations[V]] = Map.empty[Violations.Path, Violations[V]],
) {
  import Violations._
  def asChild(key: String): Violations[V] = Violations[V](children = Map(Path.Key(key) -> this))
  def asChild(index: Int): Violations[V]  = Violations[V](children = Map(Path.Index(index) -> this))

  def ++(other: Violations[V]): Violations[V] = {
    Violations(
      values = values ++ other.values,
      children = other.children.foldLeft(children) { case (acc, (ko, vo)) =>
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

  def Empty[V]: Violations[V] = Violations[V]()

  implicit def monoid[V]: Monoid[Violations[V]] = Monoid.instance(Empty, { _ ++ _ })

  sealed trait Path extends Product with Serializable
  object Path {
    case class Key(value: String) extends Path
    case class Index(value: Int)  extends Path
  }
}
