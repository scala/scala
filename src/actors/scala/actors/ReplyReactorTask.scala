/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors

/** <p>
 *    The class <code>ReplyReactorTask</code>.
 *  </p>
 *
 *  @author Philipp Haller
 */
private[actors] class ReplyReactorTask[T >: Null <: ReplyReactor](reactor: T, fun: () => Unit) extends ReactorTask[ReplyReactor](reactor, fun) {

  var saved: ReplyReactor = _

  protected override def beginExecution() {
    saved = Actor.tl.get
    Actor.tl set reactor
  }

  protected override def suspendExecution() {
    Actor.tl set saved
  }

}
