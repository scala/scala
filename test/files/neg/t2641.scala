import scala.collection._
import scala.collection.generic._
import scala.collection.mutable.Builder


abstract class ManagedSeqStrict[+A]
        extends Traversable[A]
        with GenericTraversableTemplate[A, ManagedSeqStrict]
{
    override def companion: GenericCompanion[ManagedSeqStrict] = null
   
    override def foreach[U](f: A => U): Unit =
        null
}

trait ManagedSeq[+A, +Coll]
        extends ManagedSeqStrict[A]
        with TraversableView[A, ManagedSeqStrict[A]]
        with TraversableViewLike[A, ManagedSeqStrict[A], ManagedSeq[A]]
{ self =>
   
    override def underlying = throw new Exception("no underlying")
   
  //trait Transformed[+B] extends ManagedSeq[B] with super.Transformed[B]
  trait Transformed[+B] extends ManagedSeq[B, Coll] with super.Transformed[B]

  trait Sliced extends Transformed[A] with super.Sliced {
    override def managedIterator = self.managedIterator slice (from, until)
  }

}
