object Test extends App {
  println("foo_targs:")
  new Macros[Int]().foo_targs[String](42)
}