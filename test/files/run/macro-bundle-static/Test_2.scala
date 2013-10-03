object Test extends App {
  println(Macros.mono)
  println(Macros.poly[Int])
  println(new Enclosing.Impl{val c = ???}.weird)
  println(pkg.Macros.mono)
  println(pkg.Macros.poly[Int])
  println(new pkg.Enclosing.Impl{val c = ???}.weird)
}