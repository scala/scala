/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
package scala.collection.immutable

import generic._

/** A subtrait of collection.Sequence which represents sequences
 *  that cannot be mutated.
 */
trait Sequence[+A] extends Iterable[A]
                      with collection.Sequence[A]
                      with TraversableClass[A, Sequence]
                      with SequenceTemplate[A, Sequence[A]] {
  override def companion: Companion[Sequence] = Sequence

  override def hashCode = (Sequence.hashSeed /: this)(_ * 41 + _.hashCode)
}

object Sequence extends SequenceFactory[Sequence] {
  private val hashSeed = "Sequence".hashCode
  implicit def builderFactory[A]: BuilderFactory[A, Sequence[A], Coll] = new VirtualBuilderFactory[A]
  def newBuilder[A]: Builder[A, Sequence[A]] = new mutable.ListBuffer
}
