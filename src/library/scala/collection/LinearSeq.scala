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

import generic._
import mutable.Builder

/** A base trait for linear sequences.
 *
 *  $linearSeqInfo
 *
 *  @define  linearSeqInfo
 *  Linear sequences have reasonably efficient `head`, `tail`, and `isEmpty` methods.
 *  If these methods provide the fastest way to traverse the collection, a 
 *  collection `Coll` that extends this trait should also extend
 *  `LinearSeqOptimized[A, Coll[A]]`.
 */
trait LinearSeq[+A] extends Seq[A]
                            with GenericTraversableTemplate[A, LinearSeq]
                            with LinearSeqLike[A, LinearSeq[A]] {
  override def companion: GenericCompanion[LinearSeq] = LinearSeq
  override def seq: LinearSeq[A] = this
}

/** $factoryInfo
 *  The current default implementation of a $Coll is a `List`.
 *  @define coll linear sequence
 *  @define Coll `LinearSeq`
 */
object LinearSeq extends SeqFactory[LinearSeq] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, LinearSeq[A]] = ReusableCBF.asInstanceOf[GenericCanBuildFrom[A]]
  def newBuilder[A]: Builder[A, LinearSeq[A]] = immutable.LinearSeq.newBuilder[A]
}
