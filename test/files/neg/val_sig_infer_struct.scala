class C {
  def f: Object = this
}

class D extends C {
  override val f = new Object { def foo = 1 }
  def bar = f.foo
}