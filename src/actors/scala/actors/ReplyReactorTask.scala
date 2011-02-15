/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors

/**
 *  @author Philipp Haller
 */
private[actors] class ReplyReactorTask(reactor: ReplyReactor,
                                       fun: () => Unit,
                                       handler: PartialFunction[Any, Any],
                                       msg: Any)
  extends ReactorTask(reactor, fun, handler, msg) {

  var saved: ReplyReactor = _

  protected override def beginExecution() {
    saved = Actor.tl.get
    Actor.tl set reactor
  }

  protected override def suspendExecution() {
    Actor.tl set saved
  }

}
