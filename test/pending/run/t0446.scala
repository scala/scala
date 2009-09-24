object Test extends Application {
  val a = Array(1,3).takeWhile(_ < 2)
  val b = Array(1,3).dropWhile(_ < 2)
  println(a.toString)
  println(b.toString)
}
