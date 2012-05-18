/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent

import scala.concurrent.util.Duration

sealed trait CanAwait

object Await {
  private[concurrent] implicit val canAwaitEvidence = new CanAwait {}
  
  def ready[T](awaitable: Awaitable[T], atMost: Duration): awaitable.type = {
    blocking(awaitable, atMost)
    awaitable
  }
  
  def result[T](awaitable: Awaitable[T], atMost: Duration): T = {
    blocking(awaitable, atMost)
  }
}
