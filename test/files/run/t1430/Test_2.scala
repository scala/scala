package s {
  object Boop extends j.Bar_1 {
    def foo() {}
    def bar() {}
  }
  class Baz(x: j.Bar_1) {
    x.foo
    override def toString = "Baz"
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    println(new s.Baz(s.Boop))
  }
}
