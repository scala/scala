/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.concurrent

/** The <code>TaskRunner</code> trait...
 *
 *  @author Philipp Haller
 */
trait TaskRunner {

  type Task[T]

  implicit def functionAsTask[S](fun: () => S): Task[S]

  def execute[S](task: Task[S]): Unit

  def shutdown(): Unit

}
