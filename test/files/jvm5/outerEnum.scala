import enums._

object Test extends Application {
  def foo {
    val res: OuterEnum.Foo = OuterEnum.Foo.Bar
    println(res)
  }
  foo
}
