object Test extends App {
  class Bob[T]
  implicit def foo2bar[T](xs: List[T]): Bob[T] = new Bob[T]
  var x: Bob[Int] = null
  x = List(1,2,3)
}
