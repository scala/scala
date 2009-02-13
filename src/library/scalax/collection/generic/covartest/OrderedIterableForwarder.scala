/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: IterableProxy.scala 15458 2008-06-28 20:23:22Z stepancheg $


package scalax.collection.generic.covartest

/** This trait implements a forwarder for iterable objects. It forwards
 *  all calls to a different iterable object, except for
 *
 *    - toString, hashCode, equals, stringPrefix
 *    - newBuilder, view
 *    - all calls creating a new iterable object of the same kind
 *
 *  The above methods are forwarded by subclass IterableProxy
 *
 *  @author  Martin Odersky
 *  @version 2.8
 */
trait OrderedIterableForwarder[+A] extends OrderedIterable[A] with IterableForwarder[A] {

  /** The iterable object to which calls are forwarded */
  protected def underlying: OrderedIterable[A]

  // Iterable delegates
  // Iterable methods could be printed by  cat IterableTemplate.scala | sed -n '/trait Iterable/,$ p' | egrep '^  (override )?def'

  override def last: A = underlying.last
  override def lastOption: Option[A] = underlying.lastOption
  override def sameElements[B >: A](that: OrderedIterable[B]): Boolean = underlying.sameElements(that)
}
