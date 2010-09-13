package scala.collection.parallel




import scala.concurrent.forkjoin._










/** A trait that declares task execution capabilities used
 *  by parallel collections. Parallel collections inherit a subtrait
 *  of this trait.
 *
 *  One implementation trait of `TaskExecution` is `ForkJoinTaskExecution`.
 */
trait Tasks {

  /** A task abstraction which allows starting a task with `start`,
   *  waiting for it to finish with `sync` and attempting to cancel
   *  the task with `tryCancel`.
   *  It also defines a method `leaf` which must be called once the
   *  the task is started and defines what this task actually does.
   *  Method `split` allows splitting this task into smaller subtasks,
   *  and method `shouldSplitFurther` decides if the task should be
   *  partitioned further.
   *  Method `merge` allows merging the results of the 2 tasks. It updates
   *  the result of the receiver.
   *  Finally, it defines the task result of type `U`.
   */
  trait Task[R, +Tp] {
    type Result = R
    def repr = this.asInstanceOf[Tp]
    /** Code that gets called after the task gets started - it may spawn other tasks instead of calling `leaf`. */
    def compute
    /** Body of the task - non-divisible unit of work done by this task. Optionally is provided with the result from the previous task
     *  or `None` if there was no previous task.
     */
    def leaf(result: Option[R])
    /** Start task. */
    def start
    /** Wait for task to finish. */
    def sync
    /** Try to cancel the task.
     *  @return     `true` if cancellation is successful.
     */
    def tryCancel: Boolean
    /** A result that can be accessed once the task is completed. */
    def result: R
    /** Decides whether or not this task should be split further. */
    def shouldSplitFurther: Boolean
    /** Splits this task into a list of smaller tasks. */
    protected[this] def split: Seq[Task[R, Tp]]
    /** Read of results of `that` task and merge them into results of this one. */
    protected[this] def merge(that: Tp) {}
  }

  type TaskType[R, +Tp] <: Task[R, Tp]
  type ExecutionEnvironment

  var environment: ExecutionEnvironment

  /** Executes a task and waits for it to finish. */
  def executeAndWait[R, Tp](task: TaskType[R, Tp])

  /** Executes a result task, waits for it to finish, then returns its result. */
  def executeAndWaitResult[R, Tp](task: TaskType[R, Tp]): R

  /** Retrieves the parallelism level of the task execution environment. */
  def parallelismLevel: Int

}


/** This trait implements scheduling by employing
 *  an adaptive work stealing technique.
 */
trait AdaptiveWorkStealingTasks extends Tasks {

  trait Task[R, Tp] extends super.Task[R, Tp] {
    var next: Task[R, Tp] = null
    var shouldWaitFor = true
    var result: R

    def split: Seq[Task[R, Tp]]

    /** The actual leaf computation. */
    def leaf(result: Option[R]): Unit

    def compute = if (shouldSplitFurther) internal else leaf(None)

    def internal = {
      var last = spawnSubtasks

      last.leaf(None)
      result = last.result

      while (last.next != null) {
        val lastresult = Option(last.result)
        last = last.next
        if (last.tryCancel) last.leaf(lastresult) else last.sync
        merge(last.repr)
      }
    }

    def spawnSubtasks = {
      var last: Task[R, Tp] = null
      var head: Task[R, Tp] = this
      do {
        val subtasks = head.split
        head = subtasks.head
        for (t <- subtasks.tail.reverse) {
          t.next = last
          last = t
          t.start
        }
      } while (head.shouldSplitFurther);
      head.next = last
      head
    }

    def printChain = {
      var curr = this
      var chain = "chain: "
      while (curr != null) {
        chain += curr + " ---> "
        curr = curr.next
      }
      println(chain)
    }
  }

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

  trait Task[R, +Tp] extends RecursiveAction with super.Task[R, Tp] {
    def start = fork
    def sync = join
    def tryCancel = tryUnfork
    var result: R
  }

  type TaskType[R, +Tp] = Task[R, Tp]
  type ExecutionEnvironment = ForkJoinPool

  /** The fork/join pool of this collection.
   */
  def forkJoinPool: ForkJoinPool = environment
  var environment = ForkJoinTasks.defaultForkJoinPool

  /** Executes a task on a fork/join pool and waits for it to finish.
   *
   *  $fjdispatch
   */
  def executeAndWait[R, Tp](fjtask: Task[R, Tp]) {
    if (currentThread.isInstanceOf[ForkJoinWorkerThread]) {
      fjtask.fork
    } else {
      forkJoinPool.execute(fjtask)
    }
    fjtask.join
  }

  /** Executes a task on a fork/join pool and waits for it to finish.
   *  Returns its result when it does.
   *
   *  $fjdispatch
   *
   *  @return    the result of the task
   */
  def executeAndWaitResult[R, Tp](fjtask: Task[R, Tp]): R = {
    if (currentThread.isInstanceOf[ForkJoinWorkerThread]) {
      fjtask.fork
    } else {
      forkJoinPool.execute(fjtask)
    }
    fjtask.join
    fjtask.result
  }

  def parallelismLevel = forkJoinPool.getParallelism

}

object ForkJoinTasks {
  val defaultForkJoinPool = new ForkJoinPool
  defaultForkJoinPool.setParallelism(Runtime.getRuntime.availableProcessors)
  defaultForkJoinPool.setMaximumPoolSize(Runtime.getRuntime.availableProcessors)
}


/* Some boilerplate due to no deep mixin composition. Not sure if it can be done differently without them.
 */
trait AdaptiveWorkStealingForkJoinTasks extends ForkJoinTasks with AdaptiveWorkStealingTasks {

  trait Task[R, Tp] extends super[ForkJoinTasks].Task[R, Tp] with super[AdaptiveWorkStealingTasks].Task[R, Tp]

}










