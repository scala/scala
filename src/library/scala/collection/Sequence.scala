/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Sequence.scala 16092 2008-09-12 10:37:06Z nielsen $


package scala.collection

import mutable.ListBuffer
// import immutable.{List, Nil, ::}
import generic._
import util.control.Breaks._

/** Class <code>Sequence[A]</code> represents sequences of elements
 *  of type <code>A</code>.
 *  It adds the following methods to class Iterable:
 *   `length`, `lengthCompare`, `apply`, `isDefinedAt`, `segmentLength`, `prefixLengh`,
 *   `indexWhere`, `indexOf`, `lastIndexWhere`, `lastIndexOf`, `reverse`, `reversedElements`,
 *   `startsWith`, `endsWith`, `indexOfSeq`.
 *
 *
 *  @author  Martin Odersky
 *  @author  Matthias Zenger
 *  @version 1.0, 16/07/2003
 */
trait Sequence[+A] extends PartialFunction[Int, A] with Iterable[A] with SequenceTemplate[A, Sequence[A]] {
  override protected[this] def newBuilder = Sequence.newBuilder
  override def traversableBuilder[B]: Builder[B, Sequence[B]] = Sequence.newBuilder[B]
}

object Sequence extends SequenceFactory[Sequence] {

  type Coll = Sequence[_]
  implicit def builderFactory[A]: BuilderFactory[A, Sequence[A], Coll] = new BuilderFactory[A, Sequence[A], Coll] { def apply(from: Coll) = from.traversableBuilder[A] }
  def newBuilder[A]: Builder[A, Sequence[A]] = immutable.Sequence.newBuilder[A]

  /** @deprecated use View instead
   */
  @deprecated type Projection[A] = SequenceView[A, Coll]

  /** @deprecated use Sequence(value) instead */
  @deprecated def singleton[A](value: A) = Sequence(value)

  /** Builds a singleton sequence.
   *
   * @deprecated use <code>Sequence(x)</code> instead.
   */
  @deprecated def single[A](x: A) = singleton(x)
}

