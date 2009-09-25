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
 *    Class <code>Sequence[A]</code> represents sequences of elements
 *    of type <code>A</code>.<br/>
 *    It adds the following methods to class <code>Iterable</code>:
 *    <code>length</code>, <code>lengthCompare</code>, <code>apply</code>,
 *    <code>isDefinedAt</code>, <code>segmentLength</code>,
 *    <code>prefixLength</code>, <code>indexWhere</code>, <code>indexOf</code>,
 *    <code>lastIndexWhere</code>, <code>lastIndexOf</code>, <code>reverse</code>,
 *    <code>reverseIterator</code>, <code>startsWith</code>,
 *    <code>endsWith</code>, <code>indexOfSeq</code>.
 *  </p>
 *
 *  @author  Martin Odersky
 *  @author  Matthias Zenger
 *  @version 1.0, 16/07/2003
 */
trait Sequence[+A] extends PartialFunction[Int, A]
                      with Iterable[A]
                      with GenericTraversableTemplate[A, Sequence]
                      with SequenceLike[A, Sequence[A]] {
  override def companion: GenericCompanion[Sequence] = Sequence
}

/** Factory object for <code>Sequence</code> trait.
 *
 *  @author  Martin Odersky
 *  @version 2.8
 */
object Sequence extends SequenceFactory[Sequence] {

  private[collection] val hashSeed = "Sequence".hashCode

  implicit def builderFactory[A]: BuilderFactory[A, Sequence[A], Coll] = new VirtualBuilderFactory[A]
  def newBuilder[A]: Builder[A, Sequence[A]] = immutable.Sequence.newBuilder[A]

  @deprecated("use View instead")
  type Projection[A] = SequenceView[A, Coll]

  @deprecated("use Sequence(value) instead")
  def singleton[A](value: A) = Sequence(value)

  /** Builds a singleton sequence. */
  @deprecated("use <code>Sequence(x)</code> instead.")
  def single[A](x: A) = singleton(x)
}

