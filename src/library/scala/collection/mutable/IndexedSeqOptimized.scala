/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.collection
package mutable
import generic._

/** A subtrait of scala.collection.IndexedSeq which represents sequences
 *  that can be mutated.
 *
 *  @since 2.8
 */
trait IndexedSeqOptimized[A, +Repr] extends Any with IndexedSeqLike[A, Repr] with scala.collection.IndexedSeqOptimized[A, Repr]
