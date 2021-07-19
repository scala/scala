// here we test unpickling a sealed child in another tasty file
package dottyi3149

object TestFooMatch {
  def foo(f: Foo): Unit = f match {
    case f: Foo.Bar => ()
  }
}
