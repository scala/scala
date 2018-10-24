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

/** A subtrait of `collection.IndexedSeq` which represents sequences
 *  that can be mutated.
 *
 *  $indexedSeqInfo
 */
trait IndexedSeq[A] extends Seq[A]
                   with scala.collection.IndexedSeq[A]
                   with GenericTraversableTemplate[A, IndexedSeq]
                   with IndexedSeqLike[A, IndexedSeq[A]] {
  override def companion: GenericCompanion[IndexedSeq]  = IndexedSeq
  override def seq: IndexedSeq[A] = this
}

/** $factoryInfo
 *  The current default implementation of a $Coll is an `ArrayBuffer`.
 *  @define coll mutable indexed sequence
 *  @define Coll `mutable.IndexedSeq`
 */
object IndexedSeq extends SeqFactory[IndexedSeq] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, IndexedSeq[A]] = ReusableCBF.asInstanceOf[GenericCanBuildFrom[A]]
  def newBuilder[A]: Builder[A, IndexedSeq[A]] = new ArrayBuffer[A]
}
