// scalac: -Ydelambdafy:method-ref
object Test {
  def main(args: Array[String]): Unit = {
    val str = ""
    val foo = new Foo()
    use(foo.bar(str))
  }

  class Foo { def bar(x: Object) = Symbol("ok") }

  def use(x: => Any) = ()
}
