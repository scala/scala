/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent

import scala.language.{higherKinds, implicitConversions}

/** The `TaskRunner` trait...
 *
 *  @author Philipp Haller
 */
@deprecated("Use `ExecutionContext` instead.", "2.10.0")
private[scala] trait TaskRunner {

  type Task[T]

  implicit def functionAsTask[S](fun: () => S): Task[S]

  def execute[S](task: Task[S]): Unit

  def shutdown(): Unit
}
