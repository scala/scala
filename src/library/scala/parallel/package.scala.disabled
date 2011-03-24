package scala



import scala.concurrent.forkjoin._


/** This package object contains various parallel operations.
 *
 *  @define invokingPar
 *  Invoking a parallel computation creates a future which will
 *  hold the result of the computation once it completes. Querying
 *  the result of a future before its parallel computation has completed
 *  will block the caller. For all practical concerns, the dependency
 *  chain obtained by querying results of unfinished futures can have
 *  arbitrary lengths. However, care must be taken not to create a
 *  circular dependency, as this will result in a deadlock.
 *  
 *  Additionally, if the parallel computation performs a blocking call
 *  (e.g. an I/O operation or waiting for a lock) other than waiting for a future,
 *  it should do so by invoking the `block` method. This is another
 *  form of waiting that could potentially create a circular dependency,
 *  an the user should take care not to do this.
 *  
 *  Users should be aware that invoking a parallel computation has a
 *  certain overhead. Parallel computations should not be invoked for
 *  small computations, as this can lead to bad performance. A rule of the
 *  thumb is having parallel computations equivalent to a loop
 *  with 50000 arithmetic operations (at least). If a parallel computation
 *  is invoked within another parallel computation, then it should be
 *  computationally equivalent to a loop with 10000 arithmetic operations.
 */
package object parallel {
  
  private[scala] val forkjoinpool = new ForkJoinPool()
  
  private class Task[T](body: =>T) extends RecursiveTask[T] with Future[T] {
    def compute = body
    def apply() = join()
  }
  
  private final def newTask[T](body: =>T) = new Task[T](body)
  
  private final def executeTask[T](task: RecursiveTask[T]) {
    if (Thread.currentThread().isInstanceOf[ForkJoinWorkerThread]) task.fork
    else forkjoinpool.execute(task)
  }
  
  /* public methods */
  
  /** Performs a call which can potentially block execution.
   *  
   *  Example:
   *  {{{
   *    val lock = new ReentrantLock
   *    
   *    // ... do something ...
   *    
   *    blocking {
   *      if (!lock.hasLock) lock.lock()
   *    }
   *  }}}
   *  
   *  '''Note:''' calling methods that wait arbitrary amounts of time
   *  (e.g. for I/O operations or locks) may severely decrease performance
   *  or even result in deadlocks. This does not include waiting for
   *  results of futures.
   */
  def blocking[T](body: =>T): T = {
    if (Thread.currentThread().isInstanceOf[ForkJoinWorkerThread]) {
      val blocker = new ForkJoinPool.ManagedBlocker {
        @volatile var done = false
        @volatile var result: Any = _
        def block() = {
          result = body
          done = true
          true
        }
        def isReleasable() = done
      }
      ForkJoinPool.managedBlock(blocker, true)
      blocker.result.asInstanceOf[T]
    } else body
  }
  
  /** Starts a parallel computation and returns a future.
   *  
   *  $invokingPar
   *  
   *  @tparam T     the type of the result of the parallel computation
   *  @param body   the computation to be invoked in parallel
   *  @return       a future with the result
   */
  def par[T](body: =>T): Future[T] = {
    val task = newTask(body)
    executeTask(task)
    task
  }
  
  /** Starts 2 parallel computations and returns a future.
   *  
   *  $invokingPar
   *
   *  @tparam T1    the type of the result of 1st the parallel computation
   *  @tparam T2    the type of the result of 2nd the parallel computation
   *  @param b1     the 1st computation to be invoked in parallel
   *  @param b2     the 2nd computation to be invoked in parallel
   *  @return       a tuple of futures corresponding to parallel computations
   */
  def par[T1, T2](b1: =>T1, b2: =>T2): (Future[T1], Future[T2]) = {
    val t1 = newTask(b1)
    executeTask(t1)
    val t2 = newTask(b2)
    executeTask(t2)
    (t1, t2)
  }
  
  /** Starts 3 parallel computations and returns a future.
   *  
   *  $invokingPar
   *
   *  @tparam T1    the type of the result of 1st the parallel computation
   *  @tparam T2    the type of the result of 2nd the parallel computation
   *  @tparam T3    the type of the result of 3rd the parallel computation
   *  @param b1     the 1st computation to be invoked in parallel
   *  @param b2     the 2nd computation to be invoked in parallel
   *  @param b3     the 3rd computation to be invoked in parallel
   *  @return       a tuple of futures corresponding to parallel computations
   */
  def par[T1, T2, T3](b1: =>T1, b2: =>T2, b3: =>T3): (Future[T1], Future[T2], Future[T3]) = {
    val t1 = newTask(b1)
    executeTask(t1)
    val t2 = newTask(b2)
    executeTask(t2)
    val t3 = newTask(b3)
    executeTask(t3)
    (t1, t2, t3)
  }
  
  /** Starts 4 parallel computations and returns a future.
   *  
   *  $invokingPar
   *
   *  @tparam T1    the type of the result of 1st the parallel computation
   *  @tparam T2    the type of the result of 2nd the parallel computation
   *  @tparam T3    the type of the result of 3rd the parallel computation
   *  @tparam T4    the type of the result of 4th the parallel computation
   *  @param b1     the 1st computation to be invoked in parallel
   *  @param b2     the 2nd computation to be invoked in parallel
   *  @param b3     the 3rd computation to be invoked in parallel
   *  @param b4     the 4th computation to be invoked in parallel
   *  @return       a tuple of futures corresponding to parallel computations
   */
  def par[T1, T2, T3, T4](b1: =>T1, b2: =>T2, b3: =>T3, b4: =>T4): (Future[T1], Future[T2], Future[T3], Future[T4]) = {
    val t1 = newTask(b1)
    executeTask(t1)
    val t2 = newTask(b2)
    executeTask(t2)
    val t3 = newTask(b3)
    executeTask(t3)
    val t4 = newTask(b4)
    executeTask(t4)
    (t1, t2, t3, t4)
  }
  
}












