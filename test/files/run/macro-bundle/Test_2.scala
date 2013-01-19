object Test extends App {
  println(Macros.mono)
  println(Macros.poly[Int])
  println(new Impl{val c = ???}.weird)
}