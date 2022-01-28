class C extends B {
  override def bar(): Int = 1

  override def foo(): Int = {
    val f: () => Int = super.foo
    f()
  }
}

object Test {
  def main(args: Array[String]): Unit = {
   var c = new C()
   assert(c.foo() + c.bar() == 42)
  }
}
