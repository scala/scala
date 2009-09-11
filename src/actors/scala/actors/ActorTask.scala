/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.actors

import java.lang.Runnable
import java.util.concurrent.Callable

/** <p>
 *    The class <code>ActorTask</code>...
 *  </p>
 *
 *  @author Philipp Haller
 */
private[actors] class ActorTask extends Callable[Unit] with Runnable {

  private var a: Actor = null
  private var fun: () => Unit = null

  def this(a: Actor, fun: () => Unit) {
    this()
    this.a = a
    this.fun = fun
  }

  def call() = run()

  def run() {
    val saved = Actor.tl.get
    Actor.tl set a
    try {
      if (a.shouldExit) // links
        a.exit()
      try {
        try {
          fun()
        } catch {
          case e: Exception if (a.exceptionHandler.isDefinedAt(e)) =>
            a.exceptionHandler(e)
        }
      } catch {
        case _: KillActorException =>
      }
      a.kill()
    }
    catch {
      case _: SuspendActorException => {
        // do nothing
      }
      case t: Exception => {
        Debug.info(a+": caught "+t)
        a.terminated()
        // links
        a.synchronized {
          if (!a.links.isEmpty)
            a.exitLinked(t)
        }
      }
    } finally {
      Actor.tl set saved
      this.a = null
      this.fun = null
    }
  }

}
