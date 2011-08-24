object A {
  println("""This a "raw" string ending with a "double quote"""")
}

object Test extends App {
  val xs = Array(1, 2, 3)
  Console.println(xs.filter(_ >= 0).length)
}
