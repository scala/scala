package instrumented

/** Bar is the possible inline target */
class Bar {
  def bar(y: Foo, x1: Int, x2: Array[String], x3: Array[Int], x4: Gândăcel): Unit = {
    y.foo(x1)
    y.foo(x2)
    y.foo(x3)
    y.foo[Gândăcel](x4)
  }
}
