/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.concurrent

/** The <code>AsyncInvokable</code> trait...
 *
 *  @author Philipp Haller
 */
trait AsyncInvokable[-T, +R] {

  type Future[+S] <: () => S

  def !!(task: T): Future[R]

}
