object Test extends App {
  println(Macros.mono)
  println(Macros.poly[Int])
  println(new Impl(???).weird)
  println(pkg.Macros.mono)
  println(pkg.Macros.poly[Int])
  println(new pkg.Impl(???).weird)
}