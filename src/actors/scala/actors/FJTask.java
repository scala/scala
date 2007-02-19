/*
  File: Task.java

  Originally written by Doug Lea and released into the public domain.
  This may be used for any purposes whatsoever without acknowledgment.
  Thanks for the assistance and support of Sun Microsystems Labs,
  and everyone contributing, testing, and using this code.

  History:
  Date       Who                What
  7Jan1999   dl                 first release
  14jan1999  dl                 simplify start() semantics;
                                improve documentation
  18Jan1999  dl                 Eliminate useless time-based waits.
  7Mar1999   dl                 Add reset method,
                                add array-based composite operations
  27Apr1999  dl                 Rename
*/

package scala.actors;


/**
 * Abstract base class for Fork/Join Tasks.
 *
 * <p>
 * FJTasks are lightweight, stripped-down analogs of Threads.
 * Many FJTasks share the same pool of Java threads. This is
 * supported by the FJTaskRunnerGroup and FJTaskRunner classes, that
 * mainly contain
 * methods called only internally by FJTasks.
 * FJTasks support versions of the most common methods found in class Thread,
 * including start(), yield() and join(). However, they
 * don't support priorities, ThreadGroups or other bookkeeping
 * or control methods of class Thread.
 * <p>
 * FJTasks should normally be defined by subclassing and adding a run() method.
 * Alternatively, static inner class <code>Wrap(Runnable r)</code>
 * can be used to
 * wrap an existing Runnable object in a FJTask.
 * <p>
 * <code>FJTaskRunnerGroup.execute(FJTask)</code> can be used to
 * initiate a FJTask from a non-FJTask thread.
 * And <code>FJTaskRunnerGroup.invoke(FJTask)</code> can be used to initiate
 * a FJTask and then wait for it to complete before returning.
 * These are the only entry-points from normal threads to FJTasks.
 * Most FJTask methods themselves may only be called from within running FJTasks.
 * They throw ClassCastExceptions if they are not,
 * reflecting the fact that these methods
 * can only be executed using FJTaskRunner threads, not generic
 * java.lang.Threads.
 * <p>
 * There are three different ways to run a FJTask,
 * with different scheduling semantics:
 * <ul>
 *   <li> FJTask.start() (as well as FJTaskRunnerGroup.execute(FJTask))
 *         behaves pretty much like Thread.start(). It enqueues a task to be
 *         run the next time any FJTaskRunner thread is otherwise idle.
 *         It maintains standard FIFO ordering with respect to
 *         the group of worker threads.
 *   <li> FJTask.fork() (as well as the two-task spawning method,
 *         coInvoke(task1, task2), and the array version
 *         coInvoke(FJTask[] tasks)) starts a task
 *         that will be executed in
 *         procedure-call-like LIFO order if executed by the
 *         same worker thread as the one that created it, but is FIFO
 *         with respect to other tasks if it is run by
 *         other worker threads. That is, earlier-forked
 *         tasks are preferred to later-forked tasks by other idle workers.
 *         Fork() is noticeably faster than start(), but can only be
 *         used when these scheduling semantics are acceptable.
 *   <li> FJTask.invoke(FJTask) just executes the run method
 *        of one task from within another. It is the analog of a
 *        direct call.
 * </ul>
 * <p>
 * The main economies of FJTasks stem from the fact that
 * FJTasks do not support blocking operations of any kind.
 * FJTasks should just run to completion without
 * issuing waits or performing blocking IO.
 * There are several styles for creating the run methods that
 * execute as tasks, including
 * event-style methods, and pure computational methods.
 * Generally, the best kinds of FJTasks are those that in turn
 * generate other FJTasks.
 * <p>
 * There is nothing actually
 * preventing you from blocking within a FJTask, and very short waits/blocks are
 * completely well behaved. But FJTasks are not designed
 * to support arbitrary synchronization
 * since there is no way to suspend and resume individual tasks
 * once they have begun executing. FJTasks should also be finite
 * in duration -- they should not contain infinite loops.
 * FJTasks that might need to perform a blocking
 * action, or hold locks for extended periods, or
 * loop forever can instead create normal
 * java Thread objects that will do so. FJTasks are just not
 * designed to support these things.
 * FJTasks may however yield() control to allow their FJTaskRunner threads
 * to run other tasks,
 * and may wait for other dependent tasks via join(). These
 * are the only coordination mechanisms supported by FJTasks.
 * <p>
 * FJTasks, and the FJTaskRunners that execute them are not
 * intrinsically robust with respect to exceptions.
 * A FJTask that aborts via an exception does not automatically
 * have its completion flag (isDone) set.
 * As with ordinary Threads, an uncaught exception will normally cause
 * its FJTaskRunner thread to die, which in turn may sometimes
 * cause other computations being performed to hang or abort.
 * You can of course
 * do better by trapping exceptions inside the run methods of FJTasks.
 * <p>
 * The overhead differences between FJTasks and Threads are substantial,
 * especially when using fork() or coInvoke().
 * FJTasks can be two or three orders of magnitude faster than Threads,
 * at least when run on JVMs with high-performance garbage collection
 * (every FJTask quickly becomes garbage) and good native thread support.
 * <p>
 * Given these overhead savings, you might be tempted to use FJTasks for
 * everything you would use a normal Thread to do. Don't. Java Threads
 * remain better for general purpose thread-based programming. Remember
 * that FJTasks cannot be used for designs involving arbitrary blocking
 * synchronization or I/O. Extending FJTasks to support such capabilities
 * would amount to re-inventing the Thread class, and would make them
 * less optimal in the contexts that they were designed for.
 * <p>[<a href="http://gee.cs.oswego.edu/dl/classes/EDU/oswego/cs/dl/util/concurrent/intro.html"> Introduction to this package. </a>]
 * <p>
 * @see FJTaskRunner
 * @see FJTaskRunnerGroup
 **/

public abstract class FJTask implements Runnable {

  /**
   * The only status information associated with FJTasks is whether
   * the they are considered to have completed.
   * It is set true automatically within
   * FJTaskRunner methods upon completion
   * of the run method, or manually via cancel.
   **/

  private volatile boolean done; // = false;

  /**
   * Return the FJTaskRunner thread running the current FJTask.
   * Most FJTask methods are just relays to their current
   * FJTaskRunners, that perform the indicated actions.
   * @exception ClassCastException if caller thread is not a
   * running FJTask.
   **/

  public static FJTaskRunner getFJTaskRunner() {
    return (FJTaskRunner)(Thread.currentThread());
  }

  /**
   * Return the FJTaskRunnerGroup of the thread running the current FJTask.
   * @exception ClassCastException if caller thread is not a
   * running FJTask.
   **/
  public static IFJTaskRunnerGroup getFJTaskRunnerGroup() {
    return getFJTaskRunner().getGroup();
  }


  /**
   * Return true if current task has terminated or been cancelled.
   * The  method is a simple analog of the Thread.isAlive()
   * method. However, it reports true only when the task has terminated
   * or has been cancelled. It does not distinguish these two cases.
   * And there is no way to determine whether a FJTask has been started
   * or is currently executing.
   **/

  public final boolean isDone() { return done; }

  /**
   * Indicate termination. Intended only to be called by FJTaskRunner.
   * FJTasks themselves should use (non-final) method
   * cancel() to suppress execution.
   **/

  protected final void setDone() { done = true; }

  /**
   * Set the termination status of this task. This simple-minded
   * analog of Thread.interrupt
   * causes the task not to execute if it has not already been started.
   * Cancelling a running FJTask
   * has no effect unless the run method itself uses isDone()
   * to probe cancellation and take appropriate action.
   * Individual run() methods may sense status and
   * act accordingly, normally by returning early.
   **/

  public void cancel() { setDone();  }


  /**
   * Clear the termination status of this task.
   * This method is intended to be used
   * only as a means to allow task objects to be recycled. It should
   * be called only when you are sure that the previous
   * execution of this task has terminated and, if applicable, has
   * been joined by all other waiting tasks. Usage in any other
   * context is a very bad idea.
   **/

  public void reset() { done = false;  }


  /**
   * Execute this task. This method merely places the task in a
   * group-wide scheduling queue.
   * It will be run
   * the next time any TaskRunner thread is otherwise idle.
   * This scheduling  maintains FIFO ordering of started tasks
   * with respect to
   * the group of worker threads.
   * @exception ClassCastException if caller thread is not
   * running in a FJTaskRunner thread.
   **/

  public void start() { getFJTaskRunnerGroup().executeTask(this);  }


  /**
   * Arrange for execution of a strictly dependent task.
   * The task that will be executed in
   * procedure-call-like LIFO order if executed by the
   * same worker thread, but is FIFO with respect to other tasks
   * forked by this thread when taken by other worker threads.
   * That is, earlier-forked
   * tasks are preferred to later-forked tasks by other idle workers.
   * <p>
   * Fork() is noticeably
   * faster than start(). However, it may only
   * be used for strictly dependent tasks -- generally, those that
   * could logically be issued as straight method calls without
   * changing the logic of the program.
   * The method is optimized for use in parallel fork/join designs
   * in which the thread that issues one or more forks
   * cannot continue until at least some of the forked
   * threads terminate and are joined.
   * @exception ClassCastException if caller thread is not
   * running in a FJTaskRunner thread.
   **/

  public void fork() { getFJTaskRunner().push(this);  }

  /**
   * Allow the current underlying FJTaskRunner thread to process other tasks.
   * <p>
   * Spinloops based on yield() are well behaved so long
   * as the event or condition being waited for is produced via another
   * FJTask. Additionally, you must never hold a lock
   * while performing a yield or join. (This is because
   * multiple FJTasks can be run by the same Thread during
   * a yield. Since java locks are held per-thread, the lock would not
   * maintain the conceptual exclusion you have in mind.)
   * <p>
   * Otherwise, spinloops using
   * yield are the main construction of choice when a task must wait
   * for a condition that it is sure will eventually occur because it
   * is being produced by some other FJTask.  The most common
   * such condition is built-in: join() repeatedly yields until a task
   * has terminated after producing some needed results. You can also
   * use yield to wait for callbacks from other FJTasks, to wait for
   * status flags to be set, and so on. However, in all these cases,
   * you should be confident that the condition being waited for will
   * occur, essentially always because it is produced by
   * a FJTask generated by the current task, or one of its subtasks.
   *
   * @exception ClassCastException if caller thread is not
   * running in a FJTaskRunner thread.
   **/

  public static void yield() { getFJTaskRunner().taskYield(); }

  /**
   * Yield until this task isDone.
   * Equivalent to <code>while(!isDone()) yield(); </code>
   * @exception ClassCastException if caller thread is not
   * running in a FJTaskRunner thread.
   **/

  public void join() { getFJTaskRunner().taskJoin(this);   }

  /**
   * Immediately execute task t by calling its run method. Has no
   * effect if t has already been run or has been cancelled.
   * It is equivalent to  calling t.run except that it
   * deals with completion status, so should always be used
   * instead of directly calling run.
   * The method can be useful
   * when a computation has been packaged as a FJTask, but you just need to
   * directly execute its body from within some other task.
   **/

  public static void invoke(FJTask t) {
    if (!t.isDone()) {
      t.run();
      t.setDone();
    }
  }

  /**
   * Fork both tasks and then wait for their completion. It behaves as:
   * <pre>
   * task1.fork(); task2.fork(); task2.join(); task1.join();
   * </pre>
   * As a simple classic example, here is
   * a class that computes the Fibonacci function:
   * <pre>
   * public class Fib extends FJTask {
   *
   *  // Computes fibonacci(n) = fibonacci(n-1) + fibonacci(n-2);  for n> 1
   *  //          fibonacci(0) = 0;
   *  //          fibonacci(1) = 1.
   *
   *  // Value to compute fibonacci function for.
   *  // It is replaced with the answer when computed.
   *  private volatile int number;
   *
   *  public Fib(int n) { number = n; }
   *
   *  public int getAnswer() {
   *    if (!isDone()) throw new Error("Not yet computed");
   *    return number;
   *  }
   *
   *  public void run() {
   *    int n = number;
   *    if (n > 1) {
   *      Fib f1 = new Fib(n - 1);
   *      Fib f2 = new Fib(n - 2);
   *
   *      coInvoke(f1, f2); // run these in parallel
   *
   *      // we know f1 and f2 are computed, so just directly access numbers
   *      number = f1.number + f2.number;
   *    }
   *  }
   *
   *  public static void main(String[] args) { // sample driver
   *    try {
   *      int groupSize = 2;    // 2 worker threads
   *      int num = 35;         // compute fib(35)
   *      FJTaskRunnerGroup group = new FJTaskRunnerGroup(groupSize);
   *      Fib f = new Fib(num);
   *      group.invoke(f);
   *      int result = f.getAnswer();
   *      System.out.println(" Answer: " + result);
   *    }
   *    catch (InterruptedException ex) {
   *      System.out.println("Interrupted");
   *    }
   *  }
   * }
   * </pre>
   *
   * @exception ClassCastException if caller thread is not
   * running in a FJTaskRunner thread.
   **/

  public static void coInvoke(FJTask task1, FJTask task2) {
    getFJTaskRunner().coInvoke(task1, task2);
  }


  /**
   * Fork all tasks in array, and await their completion.
   * Behaviorally equivalent to:
   * <pre>
   * for (int i = 0; i &lt; tasks.length; ++i) tasks[i].fork();
   * for (int i = 0; i &lt; tasks.length; ++i) tasks[i].join();
   * </pre>
   **/

  public static void coInvoke(FJTask[] tasks) {
    getFJTaskRunner().coInvoke(tasks);
  }

  /**
   * A FJTask that holds a Runnable r, and calls r.run when executed.
   * The class is a simple utilty to allow arbitrary Runnables
   * to be used as FJTasks.
   **/

  public static class Wrap extends FJTask {
    protected final Runnable runnable;
    public Wrap(Runnable r) { runnable = r; }
    public void run() { runnable.run(); }
  }


  /**
   * A  <code>new Seq</code>, when executed,
   * invokes each task provided in the constructor,  in order.
   * The class is a simple utility
   * that makes it easier to create composite FJTasks.
   **/
  public static class Seq extends FJTask {
    protected final FJTask[] tasks;

    /**
     * Construct a Seq that, when executed, will process each of the
     * tasks in the tasks array in order
     **/
    public Seq(FJTask[] tasks) {
      this.tasks = tasks;
    }

    /**
     * Two-task constructor, for compatibility with previous release.
     **/
    public Seq(FJTask task1, FJTask task2) {
      this.tasks = new FJTask[] { task1, task2 };
    }

    public void run() {
      for (int i = 0; i < tasks.length; ++i) FJTask.invoke(tasks[i]);
    }
  }

  /**
   * Construct and return a FJTask object that, when executed, will
   * invoke the tasks in the tasks array in array order
   **/

  public static FJTask seq(FJTask[] tasks) {
    return new Seq(tasks);
  }

  /**
   * A <code>new Par</code>, when executed,
   * runs the tasks provided in the constructor in parallel using
   * coInvoke(tasks).
   * The class is a simple utility
   * that makes it easier to create composite FJTasks.
   **/
  public static class Par extends FJTask {
    protected final FJTask[] tasks;

    /**
     * Construct a Seq that, when executed, will process each of the
     * tasks in the tasks array in parallel
     **/
    public Par(FJTask[] tasks) {
      this.tasks = tasks;
    }

    /**
     * Two-task constructor, for compatibility with previous release.
     **/
    public Par(FJTask task1, FJTask task2) {
      this.tasks = new FJTask[] { task1, task2 };
    }


    public void run() {
      FJTask.coInvoke(tasks);
    }
  }


  /**
   * Construct and return a FJTask object that, when executed, will
   * invoke the tasks in the tasks array in parallel using coInvoke
   **/
  public static FJTask par(FJTask[] tasks) {
    return new Par(tasks);
  }

  /**
   * A  <code>new Seq2(task1, task2)</code>, when executed,
   * invokes task1 and then task2, in order.
   * The class is a simple utility
   * that makes it easier to create composite Tasks.
   **/
  public static class Seq2 extends FJTask {
    protected final FJTask fst;
    protected final FJTask snd;
    public Seq2(FJTask task1, FJTask task2) {
      fst = task1;
      snd = task2;
    }
    public void run() {
      FJTask.invoke(fst);
      FJTask.invoke(snd);
    }
  }

  /**
   * Construct and return a FJTask object that, when executed, will
   * invoke task1 and task2, in order
   **/

  public static FJTask seq(FJTask task1, FJTask task2) {
    return new Seq2(task1, task2);
  }

  /**
   * A <code>new Par(task1, task2)</code>, when executed,
   * runs task1 and task2 in parallel using coInvoke(task1, task2).
   * The class is a simple utility
   * that makes it easier to create composite Tasks.
   **/
  public static class Par2 extends FJTask {
    protected final FJTask fst;
    protected final FJTask snd;
    public Par2(FJTask task1, FJTask task2) {
      fst = task1;
      snd = task2;
    }
    public void run() {
      FJTask.coInvoke(fst, snd);
    }
  }


  /**
   * Construct and return a FJTask object that, when executed, will
   * invoke task1 and task2, in parallel
   **/
  public static FJTask par(FJTask task1, FJTask task2) {
    return new Par2(task1, task2);
  }

}
