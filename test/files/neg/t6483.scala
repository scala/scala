trait T extends Any {
  def foo = 1
  type X
}

class C1(val a: Any) extends AnyVal with T {
  override def foo = super[T].foo // error
}

class C2(val a: Int) extends AnyVal with T {
  override def foo = super.foo + a // okay
}

class C3(val a: Int) extends AnyVal with T {
  override def foo = C3.super.foo + a // okay
}

class C4(val a: Int) extends AnyVal with T {
  def foo {
    class Inner extends T {
      override def foo = super[T].foo + a // no (direct) error, other than that a nested class is currently illegal.
    }
  }
}
