class A {
  val x: Singleton with this.type = this
  val y: this.type = x
}

class B {
  val x = ""
  val xs: x.type with Singleton = x
  val y: x.type = xs
}
