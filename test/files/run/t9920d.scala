class C { object O }
trait T { _: C =>
  def foo {
    class D { O }
    new D
  }
}


object Test extends C with T {
  def main(args: Array[String]): Unit = {
    foo
  }
}
