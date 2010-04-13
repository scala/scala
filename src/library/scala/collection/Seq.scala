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

/** A base trait for sequences.
 *  $seqInfo
 */
trait Seq[+A] extends PartialFunction[Int, A]
                      with Iterable[A]
                      with GenericTraversableTemplate[A, Seq]
                      with SeqLike[A, Seq[A]] {
  override def companion: GenericCompanion[Seq] = Seq
}

/** $factoryInfo
 *  The current default implementation of a $Coll is a `Vector`.
 *  @define coll sequence
 *  @define Coll Seq
 */
object Seq extends SeqFactory[Seq] {

  private[collection] val hashSeed = "Seq".hashCode

  /** $genericCanBuildFromInfo */
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, Seq[A]] = new GenericCanBuildFrom[A]

  def newBuilder[A]: Builder[A, Seq[A]] = immutable.Seq.newBuilder[A]

  @deprecated("use View instead")
  type Projection[A] = SeqView[A, Coll]

  @deprecated("use Seq(value) instead")
  def singleton[A](value: A) = Seq(value)
}

