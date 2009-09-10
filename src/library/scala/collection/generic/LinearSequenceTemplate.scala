/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.generic
import scala.collection._

import mutable.ListBuffer
// import immutable.{List, Nil, ::}
import generic._
import scala.util.control.Breaks._

/** Class <code>Linear[A]</code> represents linear sequences of elements.
 *  For such sequences `isEmpty`, `head` and `tail` are guaranteed to be
 *  efficient constant time (or near so) operations.
 *  It does not add any methods to <code>Sequence</code> but overrides
 *  several methods with optimized implementations.
 *
 *  @author  Martin Odersky
 *  @author  Matthias Zenger
 *  @version 1.0, 16/07/2003
 */
trait LinearSequenceTemplate[+A, +This <: LinearSequenceTemplate[A, This] with LinearSequence[A]] extends LinearSequenceLike[A, This] {
  self: This =>
}
