package foo

class FooImpl extends Foo {
  def use(bar: Foo.Bar): Any = {
    println(bar)
  }
}

object Main extends App {
  val user = new FooImpl()
  user.use(new Foo.Bar(1))
}
