import collection.IterableOps
abstract class ManagedSeqStrict[+A]
        extends Iterable[A]
        with IterableOps[A, ManagedSeqStrict, ManagedSeqStrict[A]]
//{
//    override def companion: GenericCompanion[ManagedSeqStrict] = null
//
//    override def foreach[U](f: A => U): Unit = ()
//}

trait TraversableViewLike[+A,
+Coll,
+This <: TraversableView[A, Coll] with TraversableViewLike[A, Coll, This]]
extends Iterable[A] with IterableOps[A, Iterable, This] {
  trait Transformed[+B]
}
trait TraversableView[+A, +Coll] extends TraversableViewLike[A, Coll, TraversableView[A, Coll]]

trait ManagedSeq[+A, +Coll]
        extends ManagedSeqStrict[A]
        with TraversableView[A, ManagedSeqStrict[A]]
        with TraversableViewLike[A, ManagedSeqStrict[A], ManagedSeq[A]]
{ self =>
  //trait Transformed[+B] extends ManagedSeq[B] with super.Transformed[B]
  trait Transformed[+B] extends ManagedSeq[B, Coll] with super.Transformed[B]

  trait Sliced extends Transformed[A] with super.Sliced {
    override def managedIterator = self.managedIterator slice (0, 0)
  }

}
