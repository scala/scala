/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent



import scala.annotation.implicitNotFound
import scala.util.Duration



trait Awaitable[+T] {
  @implicitNotFound(msg = "Waiting must be done by calling `blocking(timeout) b`, where `b` is the `Awaitable` object or a potentially blocking piece of code.")
  def await(atMost: Duration)(implicit canawait: CanAwait): T
}



