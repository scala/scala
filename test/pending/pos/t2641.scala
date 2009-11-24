import scala.collection._
import scala.collection.generic._

abstract class ManagedSeqStrict[+A]
        extends Traversable[A]
        with GenericTraversableTemplate[A, ManagedSeqStrict]

trait ManagedSeq[+A, +Coll]
        extends ManagedSeqStrict[A]
        with TraversableView[A, ManagedSeqStrict[A]]
        with TraversableViewLike[A, ManagedSeqStrict[A], ManagedSeq[A/*ERROR: too few type args*/]]
{ self =>
  trait Transformed[+B] extends ManagedSeq[B, Coll] with super.Transformed[B]

  trait Sliced extends Transformed[A] with super.Sliced
}