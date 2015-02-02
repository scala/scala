package foo

class A {
  /** The other */
  def bar(x: Int): Unit = ???
  def bar(x: Str): Unit = ???
}

class B {
  (new A).bar(0)
}