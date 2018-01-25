
package t10375

trait T {
  def x: Int = 42
}
class C extends T {
  def x_=(n: Int) = ()
}

class D(final var y: Int) {
  def f() = new D(42).y += 1
}

object Test {
  val c = new C
  c.x = c.x + 10
  c.x += 10
}
