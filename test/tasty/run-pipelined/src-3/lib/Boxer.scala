package lib

object Boxer {
  def box[T](t: T): JavaBox[T] = new JavaBox(t)
}
