class Foo[@specialized T] {
  def foo(x: T) = {
    class Bar
    x.asInstanceOf[List[Bar]]
    x.asInstanceOf[Bar]
  }
}
