/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala
package collection
package mutable

import generic._

/** A subtrait of `collection.LinearSeq` which represents sequences
 *  that can be mutated.
 *  $linearSeqInfo
 *
 *  @define Coll `LinearSeq`
 *  @define coll linear sequence
 *  @see [[http://docs.scala-lang.org/overviews/collections/concrete-mutable-collection-classes.html#mutable_lists "Scala's Collection Library overview"]]
 *  section on `Mutable Lists` for more information.
 */
trait LinearSeq[A] extends Seq[A]
                           with scala.collection.LinearSeq[A]
                           with GenericTraversableTemplate[A, LinearSeq]
                           with LinearSeqLike[A, LinearSeq[A]] {
  override def companion: GenericCompanion[LinearSeq] = LinearSeq
  override def seq: LinearSeq[A] = this
}

/** $factoryInfo
 *  The current default implementation of a $Coll is a `MutableList`.
 *  @define coll mutable linear sequence
 *  @define Coll `mutable.LinearSeq`
 */
object LinearSeq extends SeqFactory[LinearSeq] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, LinearSeq[A]] = ReusableCBF.asInstanceOf[GenericCanBuildFrom[A]]
  def newBuilder[A]: Builder[A, LinearSeq[A]] = new MutableList[A]
}
