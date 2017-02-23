object Test {
  type Rep[x] = x
  //def __new[T](args: (String, Boolean, Rep[T] => Rep[_])*): Rep[T] = error("")
  val foo: Rep[Struct { val x: Int; val y: String }] = new Struct { val x: Rep[Int] = 23; val y = "y" }
}
