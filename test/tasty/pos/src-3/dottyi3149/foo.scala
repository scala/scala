// here we test unpickling a sealed child in another tasty file
package dottyi3149

sealed class Foo
object Foo {
  final class Bar extends Foo
}

class Test {
  def f = {
    class Bar extends Foo
  }
  class C {
    class Bar extends Foo
  }
  object O {
    class Bar extends Foo
  }
}
