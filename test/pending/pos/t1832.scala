// Edit by paulp: reduced.
trait Cloning {
  trait Foo
  def fn(g: Int => Unit): Foo

  implicit def mkStar(i: Int) = new { def *(a: Foo): Foo = null }

  val pool1 = 4 * fn { case i => i * 2 }
  val pool2 = 4 * fn { case i: Int => i * 2 }
}
