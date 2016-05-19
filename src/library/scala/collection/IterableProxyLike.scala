/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala
package collection

import generic._

// Methods could be printed by  cat IterableLike.scala | egrep '^  (override )?def'

/** This trait implements a proxy for Iterable objects. It forwards
 *  all calls to a different Iterable object.
 *
 *  @author  Martin Odersky
 *  @version 2.8
 *  @since   2.8
 */
@deprecated("proxying is deprecated due to lack of use and compiler-level support", "2.11.0")
trait IterableProxyLike[+A, +Repr <: IterableLike[A, Repr] with Iterable[A]]
    extends IterableLike[A, Repr]
    with TraversableProxyLike[A, Repr] {
  override def iterator: Iterator[A] = self.iterator
  override def grouped(size: Int): Iterator[Repr] = self.grouped(size)
  override def sliding(size: Int): Iterator[Repr] = self.sliding(size)
  override def sliding(size: Int, step: Int): Iterator[Repr] = self.sliding(size, step)
  override def takeRight(n: Int): Repr = self.takeRight(n)
  override def dropRight(n: Int): Repr = self.dropRight(n)
  override def zip[A1 >: A, B, That](that: GenIterable[B])(implicit bf: CanBuildFrom[Repr, (A1, B), That]): That = self.zip[A1, B, That](that)(bf)
  override def zipAll[B, A1 >: A, That](that: GenIterable[B], thisElem: A1, thatElem: B)(implicit bf: CanBuildFrom[Repr, (A1, B), That]): That = self.zipAll(that, thisElem, thatElem)(bf)
  override def zipWithIndex[A1 >: A, That](implicit bf: CanBuildFrom[Repr, (A1, Int), That]): That = self.zipWithIndex(bf)
  override def sameElements[B >: A](that: GenIterable[B]): Boolean = self.sameElements(that)
  override def view = self.view
  override def view(from: Int, until: Int) = self.view(from, until)
}
