object Test extends App {
  println(implicitly[C[Int]])
  println(implicitly[C[String]])
  println(implicitly[C[Nothing]])
}