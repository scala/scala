object Test {
  trait S { type T; val x: AnyRef }
  trait A extends S { type T <: A; val x: A = null }
  trait B extends S { type T <: B; val x: B = null }

  val a = new A{}
  val b = new B{}
  val y = if (true) a else b

  // lub of y should allow for this
  println(y.x.x)
}
