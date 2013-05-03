package scala
package collection

/** An extractor used to head/tail deconstruct sequences. */
object +: {
  def unapply[T,Coll <: SeqLike[T, Coll]](
      t: Coll with SeqLike[T, Coll]): Option[(T, Coll)] =
    if(t.isEmpty) None
    else Some(t.head -> t.tail)
}

/** An extractor used to init/last deconstruct sequences. */
object :+ {
  /** Splits a sequence into init :+ tail.
   * @return Some((init, tail)) if sequence is non-empty. None otherwise.
   */
  def unapply[T,Coll <: SeqLike[T, Coll]](
      t: Coll with SeqLike[T, Coll]): Option[(Coll, T)] =
    if(t.isEmpty) None
    else Some(t.init -> t.last)
}

// Dummy to fool ant
private abstract class SeqExtractors