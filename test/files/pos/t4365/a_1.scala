import scala.collection._

trait SeqViewLike[+A,
                  +Coll,
                  +This <: SeqView[A, Coll] with SeqViewLike[A, Coll, This]]
  extends Seq[A]   with GenSeqViewLike[A, Coll, This]
{

  trait Transformed[+B] extends super[GenSeqViewLike].Transformed[B]

  abstract class AbstractTransformed[+B] extends Seq[B] with Transformed[B] {
    def underlying: Coll = error("")
  }

  trait Reversed extends Transformed[A] with super[GenSeqViewLike].Reversed

  protected def newReversed: Transformed[A] = new AbstractTransformed[A] with Reversed
}
