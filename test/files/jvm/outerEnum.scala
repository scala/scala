import enums._

object Test extends App {
  def foo {
    val res: OuterEnum.Foo = OuterEnum.Foo.Bar
    println(res)
  }
  foo
}
