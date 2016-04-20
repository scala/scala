/** When this files is opened within the IDE, a typing error is reported. */
class A[B] extends TestIterable[B] {
  import collection.convert.wrapAsScala._
  def iterator: other.TestIterator[Nothing] = ???

  iterator./*!*/
}

object other {
  trait TestIterator[T] {
    def hasNext: Boolean
    def next: T
  }
}
