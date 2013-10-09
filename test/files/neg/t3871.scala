class Base {
  def mkNew() = {
    val s = new Sub2
    s.foo = true
    s
  }
}

class Sub2 extends Base {
  protected var foo = false
}
