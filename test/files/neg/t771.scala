class Foo {
  def a = c(b)
  def b[List[AnyRef]] = new java.util.Iterator[List[Object]] { }
  def c[A](it:java.util.Iterator[A]) = new scala.Iterator[A]
}
