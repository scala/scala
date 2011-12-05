/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala




/** This package object contains primitives for parallel programming.
 */
package object concurrent {

  type MessageDispatcher = ExecutionContext // TODO FIXME: change futures to use execution context
  
  private[concurrent] def currentExecutionContext: ThreadLocal[ExecutionContext] = new ThreadLocal[ExecutionContext] {
    override protected def initialValue = null
  }
  
  object block {
    def on[T](body: =>T): T = on(new Blockable[T] {
      def block()(implicit cb: CanBlock) = body
    })
    
    def on[T](blockable: Blockable[T]): T = {
      currentExecutionContext.get match {
        case null => blockable.block()(null) // outside
        case x => x.blockingCall(blockable) // inside an execution context thread
      }
    }
  }
  
  def future[T](body: =>T): Future[T] = null // TODO
  
  // TODO rename appropriately and make public
  private[concurrent] def isFutureThrowable(t: Throwable) = t match {
    case e: Error => false
    case t: scala.util.control.ControlThrowable => false
    case i: InterruptException => false
    case _ => true
  }
  
}


package concurrent {
  
  private[concurrent] trait CanBlock
  
}
