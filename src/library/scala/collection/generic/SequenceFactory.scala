package scala.collection.generic

/** A template for companion objects of Sequence and subclasses thereof.
 */
abstract class SequenceFactory[CC[A] <: Sequence[A]] extends TraversableFactory[CC] {

  /** This method is called in a pattern match { case Sequence(...) => }.
   *
   *  @param x the selector value
   *  @return  sequence wrapped in an option, if this is a Sequence, otherwise none
   */
  def unapplySeq[A](x: CC[A]): Some[CC[A]] = Some(x)
}
