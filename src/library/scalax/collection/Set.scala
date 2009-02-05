/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Iterable.scala 15188 2008-05-24 15:01:02Z stepancheg $

package scalax.collection

import generic._

trait Set[A] extends OrderedIterable[A] with SetTemplate[Set, A]

/* Factory object for `Sequence` class */
object Set extends IterableFactory[Set] {
  /** The empty set */
  def apply[A](args: A*): Set[A] = null // !!!
}


