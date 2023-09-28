
class Container {
  def foo = {
    class C
  }
  def bar = {
    // anonymous class
    new T {}
  }
}

trait T
