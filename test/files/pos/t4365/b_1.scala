import scala.collection._

trait GenSeqView0[+A, +Coll]

trait GenSeqViewLike[+A,
                     +Coll,
                     +This <: GenSeqView0[A, Coll] with GenSeqViewLike[A, Coll, Nothing]]
extends GenSeq[A]  {
self =>

  trait Transformed[+B] {
    def length: Int = 0
    def apply(idx: Int): B = sys.error("")
  }

  trait Reversed extends Transformed[A] {
    def iterator: Iterator[A] = createReversedIterator

    private def createReversedIterator: Iterator[A] = {
      self.foreach(_ => ())
      null
    }
  }
}
