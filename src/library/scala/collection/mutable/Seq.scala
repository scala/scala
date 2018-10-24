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


/** A subtrait of `collection.Seq` which represents sequences
 *  that can be mutated.
 *
 *  $seqInfo
 *
 *  The class adds an `update` method to `collection.Seq`.
 *
 *  @define Coll `mutable.Seq`
 *  @define coll mutable sequence
 */
trait Seq[A] extends Iterable[A]
//                with GenSeq[A]
                with scala.collection.Seq[A]
                with GenericTraversableTemplate[A, Seq]
                with SeqLike[A, Seq[A]] {
  override def companion: GenericCompanion[Seq] = Seq
  override def seq: Seq[A] = this
}

/** $factoryInfo
 *  The current default implementation of a $Coll is an `ArrayBuffer`.
 *  @define coll mutable sequence
 *  @define Coll `mutable.Seq`
 */
object Seq extends SeqFactory[Seq] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, Seq[A]] = ReusableCBF.asInstanceOf[GenericCanBuildFrom[A]]
  def newBuilder[A]: Builder[A, Seq[A]] = new ArrayBuffer
}

/** Explicit instantiation of the `Seq` trait to reduce class file size in subclasses. */
abstract class AbstractSeq[A] extends scala.collection.AbstractSeq[A] with Seq[A]
