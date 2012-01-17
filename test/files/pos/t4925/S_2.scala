class B {
  def baz = {
    val a = new A
    val o = a.inner
    val z = o.foo
    println(z)
  }
}
