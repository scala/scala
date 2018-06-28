import enums._

object Test extends App {
  def foo {
    val res: OuterEnum_1.Foo = OuterEnum_1.Foo.Bar
    println(res)
  }
  foo
}
