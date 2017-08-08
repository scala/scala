class Outer {
  final class Inner {
    def foo: Unit = ()
  }
}
object Test {
  def main(args: Array[String]): Unit = {
    val o = new Outer
    new o.Inner().foo
  }
}
