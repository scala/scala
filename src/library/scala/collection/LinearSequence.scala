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

/** Class <code>Linear[A]</code> represents linear sequences of elements.
 *  For such sequences `isEmpty`, `head` and `tail` are guaranteed to be
 *  efficient constant time (or near so) operations.
 *  It does not add any methods to Sequence but overrides several
 *  methods with optimized implementations.
 *
 *  @author  Martin Odersky
 *  @author  Matthias Zenger
 *  @version 1.0, 16/07/2003
 */
trait LinearSequence[+A] extends Sequence[A] with LinearSequenceTemplate[A, LinearSequence[A]] {
  override protected[this] def newBuilder = LinearSequence.newBuilder
  override def traversableBuilder[B]: Builder[B, LinearSequence[B]] = LinearSequence.newBuilder[B]
}

object LinearSequence extends SequenceFactory[LinearSequence] {
  type Coll = LinearSequence[_]
  implicit def builderFactory[A]: BuilderFactory[A, LinearSequence[A], Coll] = new BuilderFactory[A, LinearSequence[A], Coll] { def apply(from: Coll) = from.traversableBuilder[A] }
  def newBuilder[A]: Builder[A, LinearSequence[A]] = immutable.LinearSequence.newBuilder[A]
}
