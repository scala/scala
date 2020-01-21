package t7216

object `package` { self =>
  def foo = self // infinite loop

  // okay
  def bar = this
  // okay
  def baz: Any = self
}

object obj { self =>
  // okay
  def foo = self
}