package scala.collection.mutable

import generic._

/** A subtrait of collection.Sequence which represents sequences
 *  that cannot be mutated.
 */
trait LinearSequence[A] extends Sequence[A] with collection.LinearSequence[A] with LinearSequenceTemplate[A, LinearSequence[A]] {
  override protected[this] def newBuilder = LinearSequence.newBuilder
  override def traversableBuilder[B]: Builder[B, LinearSequence[B], Any] = LinearSequence.newBuilder[B]
}

object LinearSequence extends SequenceFactory[LinearSequence] {
  type Coll = LinearSequence[_]
  implicit def builderFactory[A]: BuilderFactory[A, LinearSequence[A], Coll] = new BuilderFactory[A, LinearSequence[A], Coll] { def apply(from: Coll) = from.traversableBuilder[A] }
  def newBuilder[A]: Builder[A, LinearSequence[A], Any] = new MutableList[A]
}
