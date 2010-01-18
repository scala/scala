class C(x: Int) { def foo = x }

object Test {
  val c = new C(0)
  import c.x
  println(x)
}

