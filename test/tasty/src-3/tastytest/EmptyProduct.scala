package tastytest

final class EmptyProduct extends Product {
  override def productArity = 0
  override final val productPrefix = "EmptyProduct"
  override def productElement(n: Int) = throw new IndexOutOfBoundsException(n.toString())
  def canEqual(that: Any): Boolean = that.isInstanceOf[EmptyProduct]
}