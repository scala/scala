/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.util.matching

/** A DelayedPair is similar to a pair, except that the second
 *  argument is delayed.
 *
 *  @author  Thibaud Hottelier
 *  @version 1.0, 15/12/2007
 *
 *  @param a the first element
 *  @param b the second element
 */
class DelayedPair[A, B](a: A, b: () => B) extends Product2[A, B] {
  override def _1: A = a
  override def _2: B = b()
}
