/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

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
 *  @see [[http://docs.scala-lang.org/overviews/collections/concrete-mutable-collection-classes.html#mutable-lists "Scala's Collection Library overview"]]
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
