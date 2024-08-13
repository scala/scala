
object Test extends App {
  val c = new C
  val d = new D

  println(c.f(42, 42))
  println(c.f(42L, 42L))

  println(c.g(42, "42"))
  println(c.g(42L, "42"))

  println(c.h(42))
  println(c.h(42L))

  println(d.f(42, 42))
  println(d.f(42L, 42L))
}
