/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.actors

import java.lang.{InterruptedException, Runnable}

/** <p>
 *    This exception is thrown whenever an actor exits.
 *    Its purpose is to let <code>exit</code> have
 *    return type <code>Nothing</code>.
 *  </p>
 *
 *  @version 0.9.8
 *  @author Philipp Haller
 */
private[actors] class ExitActorException extends Throwable

/** <p>
 *    The abstract class <code>Reaction</code> associates
 *    an instance of an <code>Actor</code> with a
 *    <a class="java/lang/Runnable" href="" target="contentFrame">
 *    <code>java.lang.Runnable</code></a>.
 *  </p>
 *
 *  @version 0.9.8
 *  @author Philipp Haller
 */
/*private[actors]*/ class Reaction(a: Actor,
                               f: PartialFunction[Any, Unit],
                               msg: Any) extends Runnable {
  def this(a: Actor) = this(a, null, null)

  def run() {
    val saved = Actor.tl.get.asInstanceOf[Actor]
    Actor.tl.set(a)
    a.isDetached = false
    try {
      if (a.shouldExit) // links
        a.exit()
      else {
        if (f == null)
          a.act()
        else
          f(msg)
        a.kill(); a.exit()
      }
    }
    catch {
      case eae: ExitActorException => {
        Scheduler.unPendReaction
      }
      case _: SuspendActorException => {
        // do nothing (continuation is already saved)
      }
      case t: Throwable => {
        Debug.info(a+": caught "+t)
        t.printStackTrace()
        Scheduler.unPendReaction
        // links
        a.synchronized {
          if (!a.links.isEmpty)
            a.exitLinked(t)
        }
      }
    } finally {
      Actor.tl.set(saved)
    }
  }

}
