package foo
package lambdaking

class Test {
  def byname(b: => Any) = ???
  def foo: Any = {
    def bar: Any = {
      byname(bar)
    }
  }
}
