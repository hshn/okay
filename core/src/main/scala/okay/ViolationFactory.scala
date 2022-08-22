package okay

trait ViolationFactory[A] {
  def required: A
  def nonInteger(value: String): A
}

object ViolationFactory {
  def apply[A](implicit ev: ViolationFactory[A]): ViolationFactory[A] = ev
}
