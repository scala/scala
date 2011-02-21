object Test extends App {
  val res0 = Stream.cons(1, Stream.cons( { println("ouch"); 2 }, Stream.empty))
  println(res0.take(1).force)
}
