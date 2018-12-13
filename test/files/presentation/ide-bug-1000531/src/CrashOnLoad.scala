/** When this files is opened within the IDE, a typing error is reported. */
class A[B] extends TestIterable[B] {
  //import collection.convert.ImplicitConversionsToScala._
  implicit def `iterator asScala`[A](it: ju.Iterator[A]): Iterator[A] = ???
  implicit def `enumeration AsScalaIterator`[A](i: ju.Enumeration[A]): Iterator[A] = ???

  def iterator: other.TestIterator[Nothing] = ???

  iterator./*!*/
}

object other {
  trait TestIterator[T] {
    def hasNext: Boolean
    def next: T
  }
}
