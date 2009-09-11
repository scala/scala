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
 *    The class <code>ReactorTask</code>...
 *  </p>
 *
 *  @author Philipp Haller
 */
private[actors] class ReactorTask extends Callable[Unit] with Runnable {

  private var reactor: Reactor = null
  private var fun: () => Unit = null

  def this(reactor: Reactor, fun: () => Unit) {
    this()
    this.reactor = reactor
    this.fun = fun
  }

  def call() = run()

  def run() {
    val saved = Actor.tl.get
    Actor.tl set reactor
    try {
      try {
        try {
          fun()
        } catch {
          case e: Exception if (reactor.exceptionHandler.isDefinedAt(e)) =>
            reactor.exceptionHandler(e)
        }
      } catch {
        case _: KillActorException =>
      }
      reactor.kill()
    }
    catch {
      case _: SuspendActorException => {
        // do nothing (continuation is already saved)
      }
      case t: Exception => {
        Debug.info(reactor+": caught "+t)
        reactor.terminated()
      }
    } finally {
      Actor.tl set saved
      this.reactor = null
      this.fun = null
    }
  }

}
