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
private[actors] class ReactorTask[Msg >: Null](var reactor: Reactor[Msg],
                                               var fun: () => Any,
                                               var handler: PartialFunction[Msg, Any],
                                               var msg: Msg)
  extends RecursiveAction with Callable[Unit] with Runnable {

  def run() {
    try {
      beginExecution()
      try {
        try {
          if (fun eq null)
            handler(msg)
          else
            fun()
        } catch {
          case e: Exception if (reactor.exceptionHandler.isDefinedAt(e)) =>
            reactor.exceptionHandler(e)
        }
      } catch {
        case _: KillActorControl =>
      }
      reactor.kill()
    }
    catch {
      case _: SuspendActorControl =>
        // do nothing (continuation is already saved)

      case e: Exception =>
        // print message on default error stream
        val msgException = "Uncaught exception in "+reactor+"\n"
        val msgMessage   = if (msg != null) "Message: "+msg+"\n" else ""
        Debug.doWarning {
          Console.err.print(msgException + msgMessage)
          e.printStackTrace()
        }

        terminateExecution(e)
        reactor.terminated()
    } finally {
      suspendExecution()
      this.reactor = null
      this.fun = null
      this.handler = null
      this.msg = null
    }
  }

  def call() = run()

  def compute() = run()

  protected def beginExecution() {}

  protected def suspendExecution() {}

  protected def terminateExecution(e: Exception) {}

}
