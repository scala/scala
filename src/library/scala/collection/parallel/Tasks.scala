/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.collection.parallel




import scala.concurrent.forkjoin._
import scala.util.control.Breaks._


import annotation.unchecked.uncheckedVariance




/** A trait that declares task execution capabilities used
 *  by parallel collections.
 */
trait Tasks {

  private[parallel] val debugMessages = collection.mutable.ArrayBuffer[String]()

  private[parallel] def debuglog(s: String) = synchronized {
    debugMessages += s
  }

  trait Task[R, +Tp] {
    type Result = R

    def repr = this.asInstanceOf[Tp]

    /** Body of the task - non-divisible unit of work done by this task.
     *  Optionally is provided with the result from the previous completed task
     *  or `None` if there was no previous task (or the previous task is uncompleted or unknown).
     */
    def leaf(result: Option[R])

    /** A result that can be accessed once the task is completed. */
    var result: R

    /** Decides whether or not this task should be split further. */
    def shouldSplitFurther: Boolean

    /** Splits this task into a list of smaller tasks. */
    private[parallel] def split: Seq[Task[R, Tp]]

    /** Read of results of `that` task and merge them into results of this one. */
    private[parallel] def merge(that: Tp @uncheckedVariance) {}

    // exception handling mechanism
    @volatile var throwable: Throwable = null
    def forwardThrowable() = if (throwable != null) throw throwable

    // tries to do the leaf computation, storing the possible exception
    private[parallel] def tryLeaf(lastres: Option[R]) {
      try {
        tryBreakable {
          leaf(lastres)
          result = result // ensure that effects of `leaf` are visible to readers of `result`
        } catchBreak {
          signalAbort
        }
      } catch {
        case thr: Exception =>
          result = result // ensure that effects of `leaf` are visible
          throwable = thr
          signalAbort
      }
    }

    private[parallel] def tryMerge(t: Tp @uncheckedVariance) {
      val that = t.asInstanceOf[Task[R, Tp]]
      val local = result // ensure that any effects of modifying `result` are detected
      // checkMerge(that)
      if (this.throwable == null && that.throwable == null) merge(t)
      mergeThrowables(that)
    }

    private def checkMerge(that: Task[R, Tp] @uncheckedVariance) {
      if (this.throwable == null && that.throwable == null && (this.result == null || that.result == null)) {
        println("This: " + this + ", thr=" + this.throwable + "; merged with " + that + ", thr=" + that.throwable)
      } else if (this.throwable != null || that.throwable != null) {
        println("merging this: " + this + " with thr: " + this.throwable + " with " + that + ", thr=" + that.throwable)
      }
    }

    private[parallel] def mergeThrowables(that: Task[_, _]) {
      if (this.throwable != null && that.throwable != null) {
        // merge exceptions, since there were multiple exceptions
        this.throwable = this.throwable alongWith that.throwable
      } else if (that.throwable != null) this.throwable = that.throwable
      else this.throwable = this.throwable
    }

    // override in concrete task implementations to signal abort to other tasks
    private[parallel] def signalAbort() {}
  }

  trait TaskImpl[R, +Tp] {
    /** the body of this task - what it executes, how it gets split and how results are merged. */
    val body: Task[R, Tp]

    def split: Seq[TaskImpl[R, Tp]]
    /** Code that gets called after the task gets started - it may spawn other tasks instead of calling `leaf`. */
    def compute()
    /** Start task. */
    def start()
    /** Wait for task to finish. */
    def sync()
    /** Try to cancel the task.
     *  @return     `true` if cancellation is successful.
     */
    def tryCancel(): Boolean
    /** If the task has been cancelled successfully, those syncing on it may
     *  automatically be notified, depending on the implementation. If they
     *  aren't, this release method should be called after processing the
     *  cancelled task.
     *
     *  This method may be overridden.
     */
    def release() {}
  }

  protected def newTaskImpl[R, Tp](b: Task[R, Tp]): TaskImpl[R, Tp]

  /* task control */

  // safe to assume it will always have the same type,
  // because the `tasksupport` in parallel iterable is final
  var environment: AnyRef

  /** Executes a task and returns a future. Forwards an exception if some task threw it. */
  def execute[R, Tp](fjtask: Task[R, Tp]): () => R

  /** Executes a result task, waits for it to finish, then returns its result. Forwards an exception if some task threw it. */
  def executeAndWaitResult[R, Tp](task: Task[R, Tp]): R

  /** Retrieves the parallelism level of the task execution environment. */
  def parallelismLevel: Int

}



/** This trait implements scheduling by employing
 *  an adaptive work stealing technique.
 */
trait AdaptiveWorkStealingTasks extends Tasks {

  trait TaskImpl[R, Tp] extends super.TaskImpl[R, Tp] {
    @volatile var next: TaskImpl[R, Tp] = null
    @volatile var shouldWaitFor = true

    def split: Seq[TaskImpl[R, Tp]]

    def compute() = if (body.shouldSplitFurther) {
      internal()
      release()
    } else {
      body.tryLeaf(None)
      release()
    }

    def internal() = {
      var last = spawnSubtasks()
      
      last.body.tryLeaf(None)
      last.release()
      body.result = last.body.result
      body.throwable = last.body.throwable
      
      while (last.next != null) {
        // val lastresult = Option(last.body.result)
        val beforelast = last
        last = last.next
        if (last.tryCancel()) {
          // println("Done with " + beforelast.body + ", next direct is " + last.body)
          last.body.tryLeaf(Some(body.result))
          last.release()
        } else {
          // println("Done with " + beforelast.body + ", next sync is " + last.body)
          last.sync()
        }
        // println("Merging " + body + " with " + last.body)
        body.tryMerge(last.body.repr)
      }
    }
    
    def spawnSubtasks() = {
      var last: TaskImpl[R, Tp] = null
      var head: TaskImpl[R, Tp] = this
      do {
        val subtasks = head.split
        head = subtasks.head
        for (t <- subtasks.tail.reverse) {
          t.next = last
          last = t
          t.start()
        }
      } while (head.body.shouldSplitFurther);
      head.next = last
      head
    }

    def printChain() = {
      var curr = this
      var chain = "chain: "
      while (curr != null) {
        chain += curr + " ---> "
        curr = curr.next
      }
      println(chain)
    }
  }

  // specialize ctor
  protected def newTaskImpl[R, Tp](b: Task[R, Tp]): TaskImpl[R, Tp]

}


/** An implementation of tasks objects based on the Java thread pooling API. */
trait ThreadPoolTasks extends Tasks {
  import java.util.concurrent._

  trait TaskImpl[R, +Tp] extends Runnable with super.TaskImpl[R, Tp] {
    // initially, this is null
    // once the task is started, this future is set and used for `sync`
    // utb: var future: Future[_] = null
    @volatile var owned = false
    @volatile var completed = false
    
    def start() = synchronized {
      // debuglog("Starting " + body)
      // utb: future = executor.submit(this)
      executor.synchronized {
        incrTasks()
        executor.submit(this)
      }
    }
    def sync() = synchronized {
      // debuglog("Syncing on " + body)
      // utb: future.get()
      executor.synchronized {
        val coresize = executor.getCorePoolSize
        if (coresize < totaltasks) {
          executor.setCorePoolSize(coresize + 1)
          //assert(executor.getCorePoolSize == (coresize + 1))
        }
      }
      while (!completed) this.wait
    }
    def tryCancel() = synchronized {
      // utb: future.cancel(false)
      if (!owned) {
        // debuglog("Cancelling " + body)
        owned = true
        true
      } else false
    }
    def run() = {
      // utb: compute
      var isOkToRun = false
      synchronized {
        if (!owned) {
          owned = true
          isOkToRun = true
        }
      }
      if (isOkToRun) {
        // debuglog("Running body of " + body)
        compute()
      } else {
        // just skip
        // debuglog("skipping body of " + body)
      }
    }
    override def release() = synchronized {
      //println("releasing: " + this + ", body: " + this.body)
      completed = true
      executor.synchronized {
        decrTasks()
      }
      this.notifyAll
    }
  }

  protected def newTaskImpl[R, Tp](b: Task[R, Tp]): TaskImpl[R, Tp]

  var environment: AnyRef = ThreadPoolTasks.defaultThreadPool
  def executor = environment.asInstanceOf[ThreadPoolExecutor]
  def queue = executor.getQueue.asInstanceOf[LinkedBlockingQueue[Runnable]]
  @volatile var totaltasks = 0

  private def incrTasks() = synchronized {
    totaltasks += 1
  }

  private def decrTasks() = synchronized {
    totaltasks -= 1
  }

  def execute[R, Tp](task: Task[R, Tp]): () => R = {
    val t = newTaskImpl(task)

    // debuglog("-----------> Executing without wait: " + task)
    t.start()

    () => {
      t.sync()
      t.body.forwardThrowable
      t.body.result
    }
  }

  def executeAndWaitResult[R, Tp](task: Task[R, Tp]): R = {
    val t = newTaskImpl(task)

    // debuglog("-----------> Executing with wait: " + task)
    t.start()
    
    t.sync()
    t.body.forwardThrowable
    t.body.result
  }

  def parallelismLevel = ThreadPoolTasks.numCores

}

object ThreadPoolTasks {
  import java.util.concurrent._

  val numCores = Runtime.getRuntime.availableProcessors

  val tcount = new atomic.AtomicLong(0L)

  val defaultThreadPool = new ThreadPoolExecutor(
    numCores,
    Int.MaxValue,
    60L, TimeUnit.MILLISECONDS,
    new LinkedBlockingQueue[Runnable],
    new ThreadFactory {
      def newThread(r: Runnable) = {
        val t = new Thread(r)
        t.setName("pc-thread-" + tcount.incrementAndGet)
        t.setDaemon(true)
        t
      }
    },
    new ThreadPoolExecutor.CallerRunsPolicy
  )
}


/** An implementation of tasks objects based on the Java thread pooling API and synchronization using futures. */
trait FutureThreadPoolTasks extends Tasks {
  import java.util.concurrent._

  trait TaskImpl[R, +Tp] extends Runnable with super.TaskImpl[R, Tp] {
    @volatile var future: Future[_] = null

    def start() = {
      executor.synchronized {
        future = executor.submit(this)
      }
    }
    def sync() = future.get
    def tryCancel = false
    def run = {
      compute()
    }
  }

  protected def newTaskImpl[R, Tp](b: Task[R, Tp]): TaskImpl[R, Tp]

  var environment: AnyRef = FutureThreadPoolTasks.defaultThreadPool
  def executor = environment.asInstanceOf[ThreadPoolExecutor]

  def execute[R, Tp](task: Task[R, Tp]): () => R = {
    val t = newTaskImpl(task)

    // debuglog("-----------> Executing without wait: " + task)
    t.start

    () => {
      t.sync
      t.body.forwardThrowable
      t.body.result
    }
  }

  def executeAndWaitResult[R, Tp](task: Task[R, Tp]): R = {
    val t = newTaskImpl(task)

    // debuglog("-----------> Executing with wait: " + task)
    t.start

    t.sync
    t.body.forwardThrowable
    t.body.result
  }

  def parallelismLevel = FutureThreadPoolTasks.numCores

}

object FutureThreadPoolTasks {
  import java.util.concurrent._

  val numCores = Runtime.getRuntime.availableProcessors

  val tcount = new atomic.AtomicLong(0L)

  val defaultThreadPool = Executors.newCachedThreadPool()
}



/**
 * A trait describing objects that provide a fork/join pool.
 */
trait HavingForkJoinPool {
  def forkJoinPool: ForkJoinPool
}


/** An implementation trait for parallel tasks based on the fork/join framework.
 *
 *  @define fjdispatch
 *  If the current thread is a fork/join worker thread, the task's `fork` method will
 *  be invoked. Otherwise, the task will be executed on the fork/join pool.
 */
trait ForkJoinTasks extends Tasks with HavingForkJoinPool {

  trait TaskImpl[R, +Tp] extends RecursiveAction with super.TaskImpl[R, Tp] {
    def start() = fork
    def sync() = join
    def tryCancel = tryUnfork
  }

  // specialize ctor
  protected def newTaskImpl[R, Tp](b: Task[R, Tp]): TaskImpl[R, Tp]

  /** The fork/join pool of this collection.
   */
  def forkJoinPool: ForkJoinPool = environment.asInstanceOf[ForkJoinPool]
  var environment: AnyRef = ForkJoinTasks.defaultForkJoinPool

  /** Executes a task and does not wait for it to finish - instead returns a future.
   *
   *  $fjdispatch
   */
  def execute[R, Tp](task: Task[R, Tp]): () => R = {
    val fjtask = newTaskImpl(task)

    if (Thread.currentThread.isInstanceOf[ForkJoinWorkerThread]) {
      fjtask.fork
    } else {
      forkJoinPool.execute(fjtask)
    }

    () => {
      fjtask.sync
      fjtask.body.forwardThrowable
      fjtask.body.result
    }
  }

  /** Executes a task on a fork/join pool and waits for it to finish.
   *  Returns its result when it does.
   *
   *  $fjdispatch
   *
   *  @return    the result of the task
   */
  def executeAndWaitResult[R, Tp](task: Task[R, Tp]): R = {
    val fjtask = newTaskImpl(task)

    if (Thread.currentThread.isInstanceOf[ForkJoinWorkerThread]) {
      fjtask.fork
    } else {
      forkJoinPool.execute(fjtask)
    }

    fjtask.sync
    // if (fjtask.body.throwable != null) println("throwing: " + fjtask.body.throwable + " at " + fjtask.body)
    fjtask.body.forwardThrowable
    fjtask.body.result
  }

  def parallelismLevel = forkJoinPool.getParallelism

}


object ForkJoinTasks {
  val defaultForkJoinPool: ForkJoinPool = new ForkJoinPool() // scala.parallel.forkjoinpool
  // defaultForkJoinPool.setParallelism(Runtime.getRuntime.availableProcessors)
  // defaultForkJoinPool.setMaximumPoolSize(Runtime.getRuntime.availableProcessors)
}


/* Some boilerplate due to no deep mixin composition. Not sure if it can be done differently without them.
 */
trait AdaptiveWorkStealingForkJoinTasks extends ForkJoinTasks with AdaptiveWorkStealingTasks {

  class TaskImpl[R, Tp](val body: Task[R, Tp])
  extends super[ForkJoinTasks].TaskImpl[R, Tp] with super[AdaptiveWorkStealingTasks].TaskImpl[R, Tp] {
    def split = body.split.map(b => newTaskImpl(b))
  }

  def newTaskImpl[R, Tp](b: Task[R, Tp]) = new TaskImpl[R, Tp](b)

}


trait AdaptiveWorkStealingThreadPoolTasks extends ThreadPoolTasks with AdaptiveWorkStealingTasks {

  class TaskImpl[R, Tp](val body: Task[R, Tp])
  extends super[ThreadPoolTasks].TaskImpl[R, Tp] with super[AdaptiveWorkStealingTasks].TaskImpl[R, Tp] {
    def split = body.split.map(b => newTaskImpl(b))
  }

  def newTaskImpl[R, Tp](b: Task[R, Tp]) = new TaskImpl[R, Tp](b)

}








