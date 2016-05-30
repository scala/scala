/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection

/** This trait implements a proxy for iterable objects. It forwards all calls
 *  to a different iterable object.
 *
 *  @author  Martin Odersky
 *  @version 2.8
 *  @since   2.8
 */
@deprecated("proxying is deprecated due to lack of use and compiler-level support", "2.11.3")
trait IterableProxy[+A] extends Iterable[A] with IterableProxyLike[A, Iterable[A]]
