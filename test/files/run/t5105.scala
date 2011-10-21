object Test {
  def main(args: Array[String]) {
    new foo.Bar
    println("You buttered your bread. Now sleep in it!")
  }
}

package foo {
  trait Foo { def foo() {} }
  class Bar extends Baz with Foo

  abstract class Baz
  object Baz extends Foo
}
