/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Iterable.scala 15188 2008-05-24 15:01:02Z stepancheg $

package scalax.collection.mutable

import generic._

trait Sequence[A] extends OrderedIterable[A] with collection.Sequence[A] with SequenceTemplate[Sequence, A]

/* Factory object for `Sequence` class */
object Sequence extends SequenceFactory[Sequence] {
  /** The empty sequence */
  def apply[A](args: A*): Sequence[A] = ArrayBuffer.apply(args: _*) // !!! swicth to Array?
}


