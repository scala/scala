//> using options -Xsource:3 -Werror

class C {
  def f(j: J): Int = j.f(42)
  def g(k: K): Int = k.f(17)
}
object Test extends App {
  def bump(i: Int): Int = i + 1
  val c = new C
  println {(
    c.f((i: Int) => i + 1),
    c.g((i: Int) => i + 1),
    c.f(bump),
    c.g(bump),
  )}
}
