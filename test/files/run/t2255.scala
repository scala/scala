object Test extends App {
  println(LazyList.continually(LazyList(1, 2, 3)).flatten.take(6).toList)
}
