/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2010-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

/** Classes and traits inheriting the `DelayedInit` marker trait
 *  will have their initialization code rewritten as follows:
 *  `code` becomes `delayedInit(code)`.
 *
 *  Initialization code comprises all statements and all value definitions
 *  that are executed during initialization.
 *
 *  @author  Martin Odersky
 */
trait DelayedInit {
  def delayedInit(x: => Unit): Unit
}
