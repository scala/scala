package scala.actors

/**
 * This class provides a dynamic actor proxy for normal Java
 * threads.
 *
 * @version Beta2
 * @author Philipp Haller
 */
private[actors] class ActorProxy(t: Thread) extends Actor {
  def act(): Unit = {}
  /**
   Terminates execution of <code>self</code> with the following
   effect on linked actors:

   For each linked actor <code>a</code> with
   <code>trapExit</code> set to <code>true</code>, send message
   <code>Exit(self, reason)</code> to <code>a</code>.

   For each linked actor <code>a</code> with
   <code>trapExit</code> set to <code>false</code> (default),
   call <code>a.exit(reason)</code> if
   <code>!reason.equals("normal")</code>.
   */
  override def exit(reason: String): Unit = {
    exitReason = reason
    t.interrupt()
  }
}
