package scala.concurrent



import scala.concurrent.forkjoin.{ ForkJoinPool, RecursiveTask => FJTask, RecursiveAction, ForkJoinWorkerThread }
import java.util.concurrent.atomic.AtomicReferenceFieldUpdater
import scala.annotation.tailrec



/* DONE: The challenge is to make ForkJoinPromise inherit from RecursiveAction
 * to avoid an object allocation per promise. This requires turning DefaultPromise
 * into a trait, i.e., removing its constructor parameters.
 */
private[concurrent] class ForkJoinTaskImpl[T](val executionContext: ForkJoinExecutionContext, val body: () => T, val timeout: Timeout)
extends FJTask[T] with Task[T] with Future[T] {
  
  private val updater = AtomicReferenceFieldUpdater.newUpdater(classOf[ForkJoinTaskImpl[T]], classOf[FJState[T]], "state")
  @volatile private var state: State[T] = _
  
  updater.set(this, Pending(List()))
  
  private def casState(oldv: State[T], newv: State[T]) = {
    updater.compareAndSet(this, oldv, newv)
  }
  
  @tailrec private def trySucceedState(res: T): Unit = updater.get(this) match {
    case p @ Pending(cbs) => if (!casState(p, Success(res))) trySucceedState(res)
    case _ => // return
  }
  
  @tailrec private def tryFailState(t: Throwable): Unit = updater.get(this) match { 
    case p @ Pending(cbs) => if (!casState(p, Failure(t))) tryFailState(t)
    case _ => // return
  }
  
  // body of RecursiveTask
  def compute(): T = {
    try {
      val res = body()
      trySucceedState(res)
    } catch handledFutureException andThen {
      t => tryFailState(t)
    } finally tryFailState(new ExecutionException)
  }
  
  def start(): Unit = {
    Thread.currentThread match {
      case fj: ForkJoinWorkerThread if fj.pool eq executionContext.pool => fork()
      case _ => executionContext.pool.execute(this)
    }
  }
  
  def future: Future[T] = this
  
  def onComplete[U](callback: Either[Throwable, T] => U): this.type = {
    @tailrec def tryAddCallback(): Either[Throwable, T] = {
      updater.get(this) match {
        case p @ Pending(lst) =>
          val pt = p.asInstanceOf[Pending[T]]
          if (casState(pt, Pending(callback :: pt.lst))) null
          else tryAddCallback()
        case Success(res) => Right(res)
        case Failure(t) => Left(t)
      }
    }
    
    val res = tryAddCallback()
    if (res != null) dispatchTask new Runnable {
      override def run() =
        try callback(res)
        catch handledFutureException
    }
  }
  
  private def dispatchTask[U](r: Runnable) = executionContext execute r
  
  def isTimedout: Boolean = false // TODO
  
  // TODO FIXME: handle timeouts
  def await(atMost: Duration): this.type =
    await
  
  def await: this.type = {
    this.join()
    this
  }
  
  def tryCancel(): Unit =
    tryUnfork()
  
}


private[concurrent] sealed abstract class FJState[T]


case class Pending[T](callbacks: List[Either[Throwable, T] => Any]) extends FJState[T]


case class Success[T](result: T) extends FJState[T]


case class Failure[T](throwable: Throwable) extends FJState[T]


private[concurrent] final class ForkJoinExecutionContext extends ExecutionContext {
  val pool = new ForkJoinPool

  @inline
  private def executeForkJoinTask(task: RecursiveAction) {
    if (Thread.currentThread.isInstanceOf[ForkJoinWorkerThread])
      task.fork()
    else
      pool execute task
  }

  def execute(task: Runnable) {
    val action = new RecursiveAction { def compute() { task.run() } }
    executeForkJoinTask(action)
  }
  
  def makeTask[T](body: () => T)(implicit timeout: Timeout): Task[T] = {
    new ForkJoinTaskImpl(this, body, timeout)
  }
  
  def makePromise[T](timeout: Timeout): Promise[T] =
    null
  
  def blockingCall[T](body: Blockable[T]): T =
    body.block()(CanBlockEvidence)

}
