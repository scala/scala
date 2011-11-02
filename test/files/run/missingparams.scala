/** Tests the optimiser. */

final class Foo(val x: Int) {
  def filter(p: Int => Boolean) =
    if (p(x)) Some(x) else None

  // test that the closure elimination is not wrongly replacing
  // 'that' by 'this'
  def intersect(that: Foo) = 
    filter { dummy => 
//      x // dummy
      that.x > 0
    }
}

object Test extends App {
  val foo1 = new Foo(42)
  val foo2 = new Foo(-42)

  println(foo1 intersect foo2)
}
