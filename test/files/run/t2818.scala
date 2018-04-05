object Test extends App {
  println(List.range(1L, 15L).foldRight(0L)(_ + _))
  println(List.range(1L, 1000000L).foldRight(0L)(_ + _))
  println(List.fill(5)(1).foldRight(1)(_ - _))
  println(List.fill(1000000)(1).foldRight(1)(_ - _))
}
