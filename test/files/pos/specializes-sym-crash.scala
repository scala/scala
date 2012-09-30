import scala.collection._

trait Foo[+A,
                     +Coll,
                     +This <: GenSeqView[A, Coll] with GenSeqViewLike[A, Coll, This]]
extends GenSeq[A] with GenSeqLike[A, This] with GenIterableView[A, Coll] with GenIterableViewLike[A, Coll, This] {
self =>

  trait Transformed[+B] extends GenSeqView[B, Coll] with super.Transformed[B] {
    def length: Int
    def apply(idx: Int): B
    override def toString = viewToString
  }
  trait Reversed extends Transformed[A] {
    override def iterator: Iterator[A] = createReversedIterator
    def length: Int = self.length
    def apply(idx: Int): A = self.apply(length - 1 - idx)
    final override protected[this] def viewIdentifier = "R"

    private def createReversedIterator = {
      var lst = List[A]()
      for (elem <- self) lst ::= elem
      lst.iterator
    }
  }
}
