/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection

import generic._
import mutable.Builder

/** A base trait for linear sequences.
 *  $linearSeqInfo
 */
trait LinearSeq[+A] extends Seq[A]
                            with GenericTraversableTemplate[A, LinearSeq]
                            with LinearSeqLike[A, LinearSeq[A]] {
  override def companion: GenericCompanion[LinearSeq] = LinearSeq
}

/** $factoryInfo
 *  The current default implementation of a $Coll is a `Vector`.
 *  @define coll linear sequence
 *  @define Coll LinearSeq
 */
object LinearSeq extends SeqFactory[LinearSeq] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, LinearSeq[A]] = new GenericCanBuildFrom[A]
  def newBuilder[A]: Builder[A, LinearSeq[A]] = immutable.LinearSeq.newBuilder[A]
}
