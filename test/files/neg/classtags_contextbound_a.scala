object Test extends App {
  def foo[T] = Array[T]()
  println(foo[Int].getClass)
}