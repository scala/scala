
trait A[@specialized(Int) T] {
  def f: T = ???
}

object B extends A[Int] {
  override final val f = 0
}
