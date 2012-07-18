class Baz extends Inter {
  def foo(x: Int) = x + 1
}

object Test {
  def main(args: Array[String]) {
    // it's important that the type is Inter so we do not call Baz.foo(I)I directly!
    val baz: Inter = new Baz
    // here we should go through specialized version of foo and thus have zero boxing
    baz.foo(1)
    println(runtime.BoxesRunTime.integerBoxCount)
  }
}
