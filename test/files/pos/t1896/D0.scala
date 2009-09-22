package p

class X[T]

trait A {
  def m(s:X[_]) {}
}

trait B extends A {
  def f { super.m(null) }
}
