package scala.concurrent



import java.util.concurrent.atomic.AtomicReferenceFieldUpdater
import scala.concurrent.forkjoin.{ ForkJoinPool, RecursiveAction, ForkJoinWorkerThread }
import scala.util.{ Timeout, Duration }
import scala.annotation.tailrec



/* DONE: The challenge is to make ForkJoinPromise inherit from RecursiveAction
 * to avoid an object allocation per promise. This requires turning DefaultPromise
 * into a trait, i.e., removing its constructor parameters.
 */
private[concurrent] class ForkJoinTaskImpl[T](val executionContext: ForkJoinExecutionContext, val body: () => T, val timeout: Timeout)
extends RecursiveAction with Task[T] with Future[T] {
  
  private val updater = AtomicReferenceFieldUpdater.newUpdater(classOf[ForkJoinTaskImpl[T]], classOf[State[T]], "state")
  @volatile private var state: State[T] = _
  
  type Callback = Either[Throwable, T] => Any
  
  updater.set(this, Pending(List()))
  
  private def casState(oldv: State[T], newv: State[T]) = {
    updater.compareAndSet(this, oldv, newv)
  }
  
  @tailrec private def trySucceedState(res: T): List[Callback] = (updater.get(this): @unchecked) match {
    case p @ Pending(cbs) => if (!casState(p, Success(res))) trySucceedState(res) else cbs
  }
  
  @tailrec private def tryFailState(t: Throwable): List[Callback] = (updater.get(this): @unchecked) match { 
    case p @ Pending(cbs) => if (!casState(p, Failure(t))) tryFailState(t) else cbs
  }
  
  private def dispatch[U](r: Runnable) = executionContext execute r
  
  private def processCallbacks(cbs: List[Callback], r: Either[Throwable, T]) =
    for (cb <- cbs) dispatch(new Runnable {
      override def run() = cb(r)
    })
  
  def compute(): Unit = {
    var cbs: List[Callback] = null
    
    try {
      val res = body()
      processCallbacks(trySucceedState(res), Right(res))
    } catch {
      case t if isFutureThrowable(t) =>
        processCallbacks(tryFailState(t), Left(t))
      case t =>
        val ee = new ExecutionException(t)
        processCallbacks(tryFailState(ee), Left(ee))
        throw t
    }
  }
  
  def start(): Unit = {
    Thread.currentThread match {
      case fj: ForkJoinWorkerThread if fj.getPool eq executionContext.pool => fork()
      case _ => executionContext.pool.execute(this)
    }
  }
  
  def future: Future[T] = this
  
  def onComplete[U](callback: Either[Throwable, T] => U): this.type = {
    @tailrec def tryAddCallback(): Either[Throwable, T] = {
      updater.get(this) match {
        case p @ Pending(lst) =>
          val pt = p.asInstanceOf[Pending[T]]
          if (casState(pt, Pending(callback :: pt.callbacks))) null
          else tryAddCallback()
        case Success(res) => Right(res)
        case Failure(t) => Left(t)
      }
    }
    
    val res = tryAddCallback()
    if (res != null) dispatch(new Runnable {
      override def run() =
        try callback(res)
        catch handledFutureException
    })
    
    this
  }
  
  def isTimedout: Boolean = updater.get(this) match {
    case Failure(ft: FutureTimeoutException) => true
    case _ => false
  }
  
  // TODO FIXME: handle timeouts
  def await(atMost: Duration): this.type =
    await
  
  def await: this.type = {
    this.join()
    this
  }
  
  def tryCancel(): Unit =
    tryUnfork()
  
  def block()(implicit canblock: CanBlock): T = {
    join()
    (updater.get(this): @unchecked) match {
      case Success(r) => r
      case Failure(t) => throw t
    }
  }
  
}


private[concurrent] sealed abstract class State[T]


case class Pending[T](callbacks: List[Either[Throwable, T] => Any]) extends State[T]


case class Success[T](result: T) extends State[T]


case class Failure[T](throwable: Throwable) extends State[T]


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
  
  def task[T](body: () => T): Task[T] = {
    new ForkJoinTaskImpl(this, body, Timeout.never)
  }
  
  def promise[T]: Promise[T] =
    null
  
  def blockingCall[T](b: Blockable[T]): T = b match {
    case fj: ForkJoinTaskImpl[_] if fj.executionContext.pool eq pool =>
      fj.block()
    case _ =>
      var res: T = null.asInstanceOf[T]
      @volatile var blockingDone = false
      // TODO add exception handling here!
      val mb = new ForkJoinPool.ManagedBlocker {
        def block() = {
          res = b.block()(CanBlockEvidence)
          blockingDone = true
          true
        }
        def isReleasable = blockingDone
      }
      ForkJoinPool.managedBlock(mb, true)
      res
  }

}
