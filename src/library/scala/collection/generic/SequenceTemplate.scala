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

import mutable.{ListBuffer, HashMap}

// import immutable.{List, Nil, ::}
import generic._

/** Class <code>Sequence[A]</code> represents sequences of elements
 *  of type <code>A</code>.
 *  It adds the following methods to class Iterable:
 *   `length`, `lengthCompare`, `apply`, `isDefinedAt`, `segmentLength`, `prefixLength`,
 *   `indexWhere`, `indexOf`, `lastIndexWhere`, `lastIndexOf`, `reverse`, `reverseIterator`,
 *   `startsWith`, `endsWith`, `indexOfSeq`, , `zip`, `zipAll`, `zipWithIndex`.
 *
 *
 *  @author  Martin Odersky
 *  @author  Matthias Zenger
 *  @version 1.0, 16/07/2003
 */
trait SequenceTemplate[+A, +This <: SequenceTemplate[A, This] with Sequence[A]]
extends IterableTemplate[A, This]
   with SequenceLike[A, This]
