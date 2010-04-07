/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: IndexedSeqLike.scala 20129 2009-12-14 17:12:17Z odersky $


package scala.collection
package mutable
import generic._

/** A subtrait of scala.collection.IndexedSeq which represents sequences
 *  that can be mutated.
 *
 *  @since 2.8
 */
trait IndexedSeqOptimized[A, +Repr] extends IndexedSeqLike[A, Repr] with scala.collection.IndexedSeqOptimized[A, Repr]
