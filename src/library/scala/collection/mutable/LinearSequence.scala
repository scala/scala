package scala.collection.mutable

import scala.collection.generic._

/** A subtrait of collection.Sequence which represents sequences
 *  that cannot be mutated.
 */
trait LinearSequence[A] extends Sequence[A]
                           with collection.LinearSequence[A]
                           with TraversableClass[A, LinearSequence]
                           with LinearSequenceTemplate[A, LinearSequence[A]] {
  override def companion: Companion[LinearSequence] = LinearSequence
}

object LinearSequence extends SequenceFactory[LinearSequence] {
  implicit def builderFactory[A]: BuilderFactory[A, LinearSequence[A], Coll] = new VirtualBuilderFactory[A]
  def newBuilder[A]: Builder[A, LinearSequence[A]] = new MutableList[A]
}
