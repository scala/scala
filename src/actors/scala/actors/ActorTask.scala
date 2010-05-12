/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



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
    actor.synchronized { // shouldExit guarded by actor
      if (actor.shouldExit)
        actor.exit()
    }
  }

  protected override def terminateExecution(e: Throwable) {
    val senderInfo = try { Some(actor.sender) } catch {
      case _: Exception => None
    }
    val uncaught = UncaughtException(actor,
                                     if (msg != null) Some(msg) else None,
                                     senderInfo,
                                     currentThread,
                                     e)

    val todo = actor.synchronized {
      if (!actor.links.isEmpty)
        actor.exitLinked(uncaught)
      else {
        super.terminateExecution(e)
        () => {}
      }
    }
    todo()
  }

}
