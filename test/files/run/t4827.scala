object Test {
  def main(args: Array[String]): Unit = Foo.foo()
}

trait CommonTrait {
  def foo(): String = null
}

class Foo

object Foo {
  def goo() = new Foo() with CommonTrait

  def foo(): String = null
}
