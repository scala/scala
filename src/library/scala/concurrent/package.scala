/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala



import scala.util.{ Timeout, Duration }



/** This package object contains primitives for parallel programming.
 */
package object concurrent {
  
  type ExecutionException = java.util.concurrent.ExecutionException
  type CancellationException = java.util.concurrent.CancellationException
  type TimeoutException = java.util.concurrent.TimeoutException
  
  lazy val executionContext =
    new default.ExecutionContextImpl
  
  private[concurrent] def currentExecutionContext: ThreadLocal[ExecutionContext] = new ThreadLocal[ExecutionContext] {
    override protected def initialValue = null
  }
  
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
  
  /* concurrency constructs */
  
  def future[T](body: =>T)(implicit execCtx: ExecutionContext = executionContext): Future[T] =
    execCtx future body
  
  def promise[T](implicit execCtx: ExecutionContext = executionContext): Promise[T] =
    execCtx promise
  
  /** Used to block on a piece of code which potentially blocks.
   *
   *  @param body         A piece of code which contains potentially blocking or long running calls.
   *  
   *  Calling this method may throw the following exceptions:
   *  - CancellationException - if the computation was cancelled
   *  - InterruptedException - in the case that a wait within the blockable object was interrupted
   *  - TimeoutException - in the case that the blockable object timed out
   */
  def block[T](timeout: Timeout)(body: =>T): T = block(timeout, new Blockable[T] {
    def block()(implicit cb: CanBlock) = body
  })
  
  /** Blocks on a blockable object.
   *  
   *  @param blockable    An object with a `block` method which runs potentially blocking or long running calls.
   *  
   *  Calling this method may throw the following exceptions:
   *  - CancellationException - if the computation was cancelled
   *  - InterruptedException - in the case that a wait within the blockable object was interrupted
   *  - TimeoutException - in the case that the blockable object timed out
   */
  def block[T](timeout: Timeout, blockable: Blockable[T]): T = {
    currentExecutionContext.get match {
      case null => blockable.block()(null) // outside - TODO - fix timeout case
      case x => x.blockingCall(timeout, blockable) // inside an execution context thread
    }
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
