


class Baz extends Inter {
  def foo(x: Int) = x + 1
}


object Test {
  
  def main(args: Array[String]) {
    val baz = new Baz
    baz.foo(1)
    println(runtime.BoxesRunTime.integerBoxCount)
  }
  
}
