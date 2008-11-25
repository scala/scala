package scalax.collection.generic

trait SequenceFactory[CC[A] <: Sequence[A]] extends IterableFactory[CC] {

  /** This method is called in a pattern match { case Sequence(...) => }.
   *
   *  @param x the selector value
   *  @return  sequence wrapped in an option, if this is a Sequence, otherwise none
   */
  def unapplySequence[A](x: CC[A]): Some[CC[A]] = Some(x)
}
