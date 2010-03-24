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
 *    The class <code>ActorTask</code>.
 *  </p>
 *
 *  @author Philipp Haller
 */
private[actors] class ActorTask(actor: Actor,
                                fun: () => Unit,
                                handler: PartialFunction[Any, Any],
                                msg: Any)
  extends ReplyReactorTask(actor, fun, handler, msg) {

  protected override def beginExecution() {
    super.beginExecution()
    if (actor.shouldExit)
      actor.exit()
  }

  protected override def terminateExecution(e: Exception) {
    actor.synchronized {
      if (!actor.links.isEmpty)
        actor.exitLinked(e)
    }
  }

}
