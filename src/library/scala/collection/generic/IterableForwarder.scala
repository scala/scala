/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.collection.generic
import scala.collection._

import collection.mutable.Buffer

/** <p>
 *    This trait implements a forwarder for iterable objects. It forwards
 *    all calls to a different iterable object, except for
 *  </p>
 *  <ul>
 *    <li><code>toString</code>, <code>hashCode</code>, <code>equals</code>,
 *      <code>stringPrefix</code></li>
 *    <li><code>newBuilder</code>, <code>view</code></li>
 *    <li>all calls creating a new iterable object of the same kind</li>
 *  </ul>
 *  <p>
 *    The above methods are forwarded by subclass <a href="../IterableProxy.html"
 *    target="ContentFrame"><code>IterableProxy</code></a>.
 *  </p>
 *
 *  @author  Martin Odersky
 *  @version 2.8
 *  @since   2.8
 */
trait IterableForwarder[+A] extends Iterable[A] with TraversableForwarder[A] {

  /** The iterable object to which calls are forwarded */
  protected def underlying: Iterable[A]

  // Iterable delegates
  // Iterable methods could be printed by  cat IterableLike.scala | sed -n '/trait Iterable/,$ p' | egrep '^  (override )?def'

  override def iterator: Iterator[A] = underlying.iterator
  override def sameElements[B >: A](that: GenIterable[B]): Boolean = underlying.sameElements(that)
}
