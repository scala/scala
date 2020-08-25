// scalac: -Ydelambdafy:method-ref
object Test {
  class Foo { def bar() = () }
  def main(args: Array[String]): Unit =
    Option(new Foo()).foreach(_.bar())
}
