package okay

trait ViolationFactory[A] {
  def required: A
  def nonInteger(value: String): A
  def tooShortString(value: String, min: Int): A
  def tooLongString(value: String, max: Int): A
}

object ViolationFactory {
  def apply[A](implicit ev: ViolationFactory[A]): ViolationFactory[A] = ev
}
