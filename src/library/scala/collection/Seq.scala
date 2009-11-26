/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection

import generic._
import mutable.Builder

/** <p>
 *    Class <code>Seq[A]</code> represents sequences of elements
 *    of type <code>A</code>.<br/>
 *    It adds the following methods to class <code>Iterable</code>:
 *    <code>length</code>, <code>lengthCompare</code>, <code>apply</code>,
 *    <code>isDefinedAt</code>, <code>segmentLength</code>,
 *    <code>prefixLength</code>, <code>indexWhere</code>, <code>indexOf</code>,
 *    <code>lastIndexWhere</code>, <code>lastIndexOf</code>, <code>reverse</code>,
 *    <code>reverseIterator</code>, <code>startsWith</code>,
 *    <code>endsWith</code>, <code>indexOfSlice</code>.
 *  </p>
 *
 *  @author  Martin Odersky
 *  @author  Matthias Zenger
 *  @version 1.0, 16/07/2003
 */
trait Seq[+A] extends PartialFunction[Int, A]
                      with Iterable[A]
                      with GenericTraversableTemplate[A, Seq]
                      with SeqLike[A, Seq[A]] {
  override def companion: GenericCompanion[Seq] = Seq
}

/** Factory object for <code>Seq</code> trait.
 *
 *  @author  Martin Odersky
 *  @version 2.8
 */
object Seq extends SeqFactory[Seq] {

  private[collection] val hashSeed = "Seq".hashCode

  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, Seq[A]] = new GenericCanBuildFrom[A]
  def newBuilder[A]: Builder[A, Seq[A]] = immutable.Seq.newBuilder[A]

  @deprecated("use View instead")
  type Projection[A] = SeqView[A, Coll]

  @deprecated("use Seq(value) instead")
  def singleton[A](value: A) = Seq(value)
}

