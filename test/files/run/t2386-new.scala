object Test extends App {
  val a = Array(Array(1, 2), Array("a","b"))
  println("a(0) = Array(" + (a(0) mkString ", ") + ")")
  println("a(1) = Array(" + (a(1) map (s => "\"" + s + "\"") mkString ", ") + ")")
}
