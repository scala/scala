trait Cloning {
  trait Foo
  def fn(g: Any => Unit): Foo

  implicit def mkStar(i: Int) = new { def *(a: Foo): Foo = null }

  val pool = 4 * fn { case ghostSYMBOL: Int => ghostSYMBOL * 2 }
}