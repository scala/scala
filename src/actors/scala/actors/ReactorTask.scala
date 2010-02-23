/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors

import java.lang.Runnable
import java.util.concurrent.Callable

import scala.concurrent.forkjoin.RecursiveAction

/** <p>
 *    The class <code>ReactorTask</code>.
 *  </p>
 *
 *  @author Philipp Haller
 */
private[actors] class ReactorTask[T >: Null <: Reactor](var reactor: T, var fun: () => Any)
  extends RecursiveAction with Callable[Unit] with Runnable {

  def run() {
    try {
      beginExecution()
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
      case _: SuspendActorException =>
        // do nothing (continuation is already saved)

      case e: Exception =>
        Debug.info(reactor+": caught "+e)
        Debug.doInfo { e.printStackTrace() }
        reactor.terminated()
        terminateExecution(e)
    } finally {
      suspendExecution()
      this.reactor = null
      this.fun = null
    }
  }

  def call() = run()

  def compute() = run()

  protected def beginExecution() {}

  protected def suspendExecution() {}

  protected def terminateExecution(e: Exception) {}

}
