//> using options -Werror -Wunused:privates

class Foo
object Foo {
  private type Alias = Foo
  def x: Foo = new Alias
}
