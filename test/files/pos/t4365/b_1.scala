import scala.collection._

trait GenSeqViewLike[+A,
                     +Coll,
                     +This <: GenSeqView[A, Coll] with GenSeqViewLike[A, Coll, This]]
extends GenSeq[A] {
self =>

  trait Transformed[+B] {
    def length: Int = 0
    def apply(idx: Int): B = error("")
  }

  trait Reversed extends Transformed[A] {
    def iterator: Iterator[A] = createReversedIterator

    private def createReversedIterator: Iterator[A] = {
      self.foreach(_ => ())
      null
    }
  }
}
