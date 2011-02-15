/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.actors

import java.lang.Runnable
import java.util.concurrent.Callable

import scala.concurrent.forkjoin.RecursiveAction

/**
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
        if (fun eq null)
          handler(msg)
        else
          fun()
      } catch {
        case _: KillActorControl =>
          // do nothing

        case e: Exception if reactor.exceptionHandler.isDefinedAt(e) =>
          reactor.exceptionHandler(e)
      }
      reactor.kill()
    }
    catch {
      case _: SuspendActorControl =>
        // do nothing (continuation is already saved)

      case e: Throwable =>
        terminateExecution(e)
        reactor.terminated()
        if (!e.isInstanceOf[Exception])
          throw e
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

  protected def terminateExecution(e: Throwable) {
    Console.err.println(reactor+": caught "+e)
    e.printStackTrace()
  }

}
