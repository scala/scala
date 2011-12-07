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
  
  type ExecutionException = java.util.concurrent.ExecutionException
  type CancellationException = java.util.concurrent.CancellationException
  type TimeoutException = java.util.concurrent.TimeoutException
  
  private[concurrent] def currentExecutionContext: ThreadLocal[ExecutionContext] = new ThreadLocal[ExecutionContext] {
    override protected def initialValue = null
  }
  
  /** The keyword used to block on a piece of code which potentially blocks.
   *
   *  @define mayThrow
   *  Calling this method may throw the following exceptions:
   *  - CancellationException - if the computation was cancelled
   *  - InterruptedException - in the case that a wait within the blockable object was interrupted
   *  - TimeoutException - in the case that the blockable object timed out
   */
  object block {
    
    /** Blocks on a piece of code.
     *  
     *  @param body         A piece of code which contains potentially blocking or long running calls.
     *  
     *  $mayThrow
     */
    def on[T](body: =>T): T = on(new Blockable[T] {
      def block()(implicit cb: CanBlock) = body
    })
    
    /** Blocks on a blockable object.
     *  
     *  @param blockable    An object with a `block` method which runs potentially blocking or long running calls.
     *  
     *  $mayThrow
     */
    def on[T](blockable: Blockable[T]): T = {
      currentExecutionContext.get match {
        case null => blockable.block()(null) // outside
        case x => x.blockingCall(blockable) // inside an execution context thread
      }
    }
  }
  
  def future[T](body: =>T): Future[T] = null // TODO
  
  val handledFutureException: PartialFunction[Throwable, Throwable] = {
    case t: Throwable if isFutureThrowable(t) => t
  }
  
  // TODO rename appropriately and make public
  private[concurrent] def isFutureThrowable(t: Throwable) = t match {
    case e: Error => false
    case t: scala.util.control.ControlThrowable => false
    case i: InterruptedException => false
    case _ => true
  }
  
}


package concurrent {
  
  /** A timeout exception.
   *  
   *  Futures are failed with a timeout exception when their timeout expires.
   *  
   *  Each timeout exception contains an origin future which originally timed out.
   */
  class FutureTimeoutException(origin: Future[_], message: String) extends TimeoutException(message) {
    def this(origin: Future[_]) = this(origin, "Future timed out.")
  }
  
}
