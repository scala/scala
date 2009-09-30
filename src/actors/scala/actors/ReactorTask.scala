/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: ReactorTask.scala 18840 2009-09-30 15:42:18Z phaller $


package scala.actors

import java.lang.Runnable

/** <p>
 *    The class <code>ReactorTask</code>.
 *  </p>
 *
 *  @author Philipp Haller
 */
private[actors] class ReactorTask[T >: Null <: Reactor](var reactor: T, var fun: () => Unit)
  extends Runnable {

  def run() {
    val saved = Actor.tl.get
    Actor.tl set reactor
    try {
      beforeExecuting()
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
        reactor.terminated()
        afterExecuting(e)
    } finally {
      Actor.tl set saved
      this.reactor = null
      this.fun = null
    }
  }

  protected def beforeExecuting() {}

  protected def afterExecuting(e: Exception) {}

}
