object Test extends App {
  val res0 = LazyList.cons(1, LazyList.cons( { println("ouch"); 2 }, LazyList.empty))
  println(res0.take(1).force)
}
