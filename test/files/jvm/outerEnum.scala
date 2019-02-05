import enums._

object Test extends App {
  def foo: Unit = {
    val res: OuterEnum.Foo = OuterEnum.Foo.Bar
    println(res)
  }
  foo
}
