/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection

import generic.{ CanBuildFrom => CBF, _ }

/** A template trait for all iterable collections which may possibly
 *  have their operations implemented in parallel.
 *
 *  This trait contains abstract methods and methods that can be implemented
 *  directly in terms of other methods.
 *
 *  @author Martin Odersky
 *  @author Aleksandar Prokopec
 *  @since 2.9
 */
trait GenIterableLike[+A, +Repr] extends GenTraversableLike[A, Repr] {

  def iterator: Iterator[A]

  def sameElements[A1 >: A](that: GenIterable[A1]): Boolean

  def zip[A1 >: A, B, That](that: GenIterable[B])(implicit bf: CBF[Repr, (A1, B), That]): That

  def zipWithIndex[A1 >: A, That](implicit bf: CBF[Repr, (A1, Int), That]): That

  def zipAll[B, A1 >: A, That](that: GenIterable[B], thisElem: A1, thatElem: B)(implicit bf: CBF[Repr, (A1, B), That]): That

  def isEmpty = iterator.isEmpty

  def head = iterator.next
}
