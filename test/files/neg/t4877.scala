trait C { }

class A {
  def foo: AnyRef { def bar: String } = new AnyRef { def bar = 42 }
  def foo2: AnyRef { def bar: String } = new AnyRef { def bar = "abc" }
  def foo3: AnyRef { def bar(x: Int): Int } = new AnyRef { def bar(x: Int) = "abc" }
  def foo4: C { def bar(x: Int): Int ; def quux(x: Int): Int } = new C { def bar(x: Int) = 5 }
}

class B {
  type Bippy = {
    type Mom
    def bar(x: Int): Mom
    def bippy(): List[Mom]
  }

  val x: Bippy = new AnyRef {
    type Mom = String
    def bar(x: Int) = 55
    def bippy() = List(bar(55))
  }
}