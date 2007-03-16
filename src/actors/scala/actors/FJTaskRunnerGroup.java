/*
  File: FJTaskRunnerGroup.java

  Originally written by Doug Lea and released into the public domain.
  This may be used for any purposes whatsoever without acknowledgment.
  Thanks for the assistance and support of Sun Microsystems Labs,
  and everyone contributing, testing, and using this code.

  History:
  Date       Who                What
  7Jan1999   dl                 First public release
  12Jan1999  dl                 made getActiveCount public; misc minor cleanup.
  14Jan1999  dl                 Added executeTask
  20Jan1999  dl                 Allow use of priorities; reformat stats
  6Feb1999   dl                 Lazy thread starts
  27Apr1999  dl                 Renamed
*/

package scala.actors;

/**
 * A stripped down analog of a ThreadGroup used for
 * establishing and managing FJTaskRunner threads.
 * ThreadRunnerGroups serve as the control boundary separating
 * the general world of normal threads from the specialized world
 * of FJTasks.
 * <p>
 * By intent, this class does not subclass java.lang.ThreadGroup, and
 * does not support most methods found in ThreadGroups, since they
 * would make no sense for FJTaskRunner threads. In fact, the class
 * does not deal with ThreadGroups at all. If you want to restrict
 * a FJTaskRunnerGroup to a particular ThreadGroup, you can create
 * it from within that ThreadGroup.
 * <p>
 * The main contextual parameter for a FJTaskRunnerGroup is
 * the group size, established in the constructor.
 * Groups must be of a fixed size.
 * There is no way to dynamically increase or decrease the number
 * of threads in an existing group.
 * <p>
 * In general, the group size should be equal to the number
 * of CPUs on the system. (Unfortunately, there is no portable
 * means of automatically detecting the number of CPUs on a JVM, so there is
 * no good way to automate defaults.)  In principle, when
 * FJTasks are used for computation-intensive tasks, having only
 * as many threads as CPUs should minimize bookkeeping overhead
 * and contention, and so maximize throughput. However, because
 * FJTaskRunners lie atop Java threads, and in turn operating system
 * thread support and scheduling policies,
 * it is very possible that using more threads
 * than CPUs will improve overall throughput even though it adds
 * to overhead. This will always be so if FJTasks are I/O bound.
 * So it may pay to experiment a bit when tuning on particular platforms.
 * You can also use <code>setRunPriorities</code> to either
 * increase or decrease the priorities of active threads, which
 * may interact with group size choice.
 * <p>
 * In any case, overestimating group sizes never
 * seriously degrades performance (at least within reasonable bounds).
 * You can also use a value
 * less than the number of CPUs in order to reserve processing
 * for unrelated threads.
 * <p>
 * There are two general styles for using a FJTaskRunnerGroup.
 * You can create one group per entire program execution, for example
 * as a static singleton, and use it for all parallel tasks:
 * <pre>
 * class Tasks {
 *   static FJTaskRunnerGroup group;
 *   public void initialize(int groupsize) {
 *      group = new FJTaskRunnerGroup(groupSize);
 *   }
 *   // ...
 * }
 * </pre>
 * Alternatively, you can make new groups on the fly and use them only for
 * particular task sets. This is more flexible,,
 * and leads to more controllable and deterministic execution patterns,
 * but it encounters greater overhead on startup. Also, to reclaim
 * system resources, you should
 * call <code>FJTaskRunnerGroup.interruptAll</code> when you are done
 * using one-shot groups. Otherwise, because FJTaskRunners set
 * <code>Thread.isDaemon</code>
 * status, they will not normally be reclaimed until program termination.
 * <p>
 * The main supported methods are <code>execute</code>,
 * which starts a task processed by FJTaskRunner threads,
 * and <code>invoke</code>, which starts one and waits for completion.
 * For example, you might extend the above <code>FJTasks</code>
 * class to support a task-based computation, say, the
 * <code>Fib</code> class from the <code>FJTask</code> documentation:
 * <pre>
 * class Tasks { // continued
 *   // ...
 *   static int fib(int n) {
 *     try {
 *       Fib f = new Fib(n);
 *       group.invoke(f);
 *       return f.getAnswer();
 *     }
 *     catch (InterruptedException ex) {
 *       throw new Error("Interrupted during computation");
 *     }
 *   }
 * }
 * </pre>
 * <p>
 * Method <code>stats()</code> can be used to monitor performance.
 * Both FJTaskRunnerGroup and FJTaskRunner may be compiled with
 * the compile-time constant COLLECT_STATS set to false. In this
 * case, various simple counts reported in stats() are not collected.
 * On platforms tested,
 * this leads to such a tiny performance improvement that there is
 * very little motivation to bother.
 *
 * <p>[<a href="http://gee.cs.oswego.edu/dl/classes/EDU/oswego/cs/dl/util/concurrent/intro.html"> Introduction to this package. </a>]
 * <p>
 * @see FJTask
 * @see FJTaskRunner
 **/

public class FJTaskRunnerGroup implements IFJTaskRunnerGroup {

  /** The threads in this group **/
    protected /*final*/ FJTaskRunner[] threads;

  /** Group-wide queue for tasks entered via execute() **/
    /*protected*/ final LinkedQueue entryQueue = new LinkedQueue();

    public LinkedQueue getEntryQueue() {
        return entryQueue;
    }

  /** Number of threads that are not waiting for work **/
  protected int activeCount = 0;

  /** Number of threads that have been started. Used to avoid
      unecessary contention during startup of task sets.
  **/
  protected int nstarted = 0;

  /**
   * Compile-time constant. If true, various counts of
   * runs, waits, etc., are maintained. These are NOT
   * updated with synchronization, so statistics reports
   * might not be accurate.
   **/

  static final boolean COLLECT_STATS = true;
  //  static final boolean COLLECT_STATS = false;

  // for stats

  /** The time at which this ThreadRunnerGroup was constructed **/
  long initTime = 0;

  /** Total number of executes or invokes **/
  int entries = 0;

  static final int DEFAULT_SCAN_PRIORITY = Thread.MIN_PRIORITY+1;

    /* -------- Suspending -------- */

    void snapshot() throws InterruptedException {
        // set flag in all task runners to suspend
        for (int i = 0; i < threads.length; ++i) {
            FJTaskRunner t = threads[i];
            t.setSuspending(true);
        }

        // interrupt all task runners
        // assume: current thread not in threads (scheduler)
        for (int i = 0; i < threads.length; ++i) {
            Thread t = threads[i];
            t.interrupt();
        }

        // wait until all of them have terminated
        for (int i = 0; i < threads.length; ++i) {
            Thread t = threads[i];
            t.join();
        }
    }

  /**
   * Create a FJTaskRunnerGroup with the indicated number
   * of FJTaskRunner threads. Normally, the best size to use is
   * the number of CPUs on the system.
   * <p>
   * The threads in a FJTaskRunnerGroup are created with their
   * isDaemon status set, so do not normally need to be
   * shut down manually upon program termination.
   **/

  public FJTaskRunnerGroup(int groupSize) {
      //System.out.println("Creating FJTaskRunnerGroup of size "+groupSize);
    threads = new FJTaskRunner[groupSize];
    initializeThreads();
    initTime = System.currentTimeMillis();
  }


    public boolean existsTask() {
        FJTask task = null;
        /*
          Circularly traverse from a random start index.

          This differs slightly from cilk version that uses a random index
          for each attempted steal.
          Exhaustive scanning might impede analytic tractablity of
          the scheduling policy, but makes it much easier to deal with
          startup and shutdown.
        */

        FJTaskRunner[] ts = threads;
        int idx = /*victimRNG.nextInt(ts.length)*/ 0;

        for (int i = 0; i < ts.length; ++i) {
            FJTaskRunner t = ts[idx];
            if (++idx >= ts.length) idx = 0; // circularly traverse

            if (t != null) {
                task = t.take();
                if (task != null) {
                    break;
                }
            }
        }

        if (task == null) {
            task = pollEntryQueue();
        }

        if (task != null && !task.isDone()) {
            boolean quit = false;
            while (!quit) {
                quit = true;
                try {
                    // put task back into entry queue and return true
                    entryQueue.put(task);
                } catch (InterruptedException ie) {
                    quit = false;
                }
            }
            return true;
        }
        else return false;
    }

    public boolean checkPoolSize() {
        // if there is a task in the entryQueue or any of the
        // worker queues, increase thread pool size by one
        if (!entryQueue.isEmpty() ||
            existsTask()) {
            int newsize = threads.length + 1;
            FJTaskRunner[] newar = new FJTaskRunner[newsize];
            System.arraycopy(threads, 0, newar, 0, newsize-1);
            synchronized(this) {
                threads = newar;
                threads[newsize-1] = new FJTaskRunner(this);
            }
            return true;
        }
        else return false;
    }


  /**
   * Arrange for execution of the given task
   * by placing it in a work queue. If the argument
   * is not of type FJTask, it is embedded in a FJTask via
   * <code>FJTask.Wrap</code>.
   * @exception InterruptedException if current Thread is
   * currently interrupted
   **/

  public void execute(Runnable r) throws InterruptedException {
    if (r instanceof FJTask) {
      entryQueue.put((FJTask)r);
    }
    else {
      entryQueue.put(new FJTask.Wrap(r));
    }
    signalNewTask();
  }


  /**
   * Specialized form of execute called only from within FJTasks
   **/
  public void executeTask(FJTask t) {
    try {
      entryQueue.put(t);
      signalNewTask();
    }
    catch (InterruptedException ex) {
      Thread.currentThread().interrupt();
    }
  }


  /**
   * Start a task and wait it out. Returns when the task completes.
   * @exception InterruptedException if current Thread is
   * interrupted before completion of the task.
   **/

  public void invoke(Runnable r) throws InterruptedException {
    InvokableFJTask w = new InvokableFJTask(r);
    entryQueue.put(w);
    signalNewTask();
    w.awaitTermination();
  }


  /**
   * Try to shut down all FJTaskRunner threads in this group
   * by interrupting them all. This method is designed
   * to be used during cleanup when it is somehow known
   * that all threads are idle.
   * FJTaskRunners only
   * check for interruption when they are not otherwise
   * processing a task (and its generated subtasks,
   * if any), so if any threads are active, shutdown may
   * take a while, and may lead to unpredictable
   * task processing.
   **/

  public void interruptAll() {
    // paranoically interrupt current thread last if in group.
    Thread current = Thread.currentThread();
    boolean stopCurrent = false;

    for (int i = 0; i < threads.length; ++i) {
      Thread t = threads[i];
      if (t == current)
        stopCurrent = true;
      else
        t.interrupt();
    }
    if (stopCurrent)
      current.interrupt();
  }


  /**
   * Set the priority to use while a FJTaskRunner is
   * polling for new tasks to perform. Default
   * is currently Thread.MIN_PRIORITY+1. The value
   * set may not go into effect immediately, but
   * will be used at least the next time a thread scans for work.
   **/
  public synchronized void setScanPriorities(int pri) {
    for (int i = 0; i < threads.length; ++i) {
      FJTaskRunner t = threads[i];
      t.setScanPriority(pri);
      if (!t.active) t.setPriority(pri);
    }
  }


  /**
   * Set the priority to use while a FJTaskRunner is
   * actively running tasks. Default
   * is the priority that was in effect by the thread that
   * constructed this FJTaskRunnerGroup. Setting this value
   * while threads are running may momentarily result in
   * them running at this priority even when idly waiting for work.
   **/
  public synchronized void setRunPriorities(int pri) {
    for (int i = 0; i < threads.length; ++i) {
      FJTaskRunner t = threads[i];
      t.setRunPriority(pri);
      if (t.active) t.setPriority(pri);
    }
  }



  /** Return the number of FJTaskRunner threads in this group **/

  public int size() { return threads.length; }


  /**
   * Return the number of threads that are not idly waiting for work.
   * Beware that even active threads might not be doing any useful
   * work, but just spinning waiting for other dependent tasks.
   * Also, since this is just a snapshot value, some tasks
   * may be in the process of becoming idle.
   **/
  public synchronized int getActiveCount() { return activeCount; }

  /**
   * Prints various snapshot statistics to System.out.
   * <ul>
   *   <li> For each FJTaskRunner thread (labeled as T<em>n</em>, for
   *         <em>n</em> from zero to group size - 1):
   *     <ul>
   *       <li> A star "*" is printed if the thread is currently active;
   *            that is, not sleeping while waiting for work. Because
   *            threads gradually enter sleep modes, an active thread
   *            may in fact be about to sleep (or wake up).
   *       <li> <em>Q Cap</em> The current capacity of its task queue.
   *       <li> <em>Run</em> The total number of tasks that have been run.
   *       <li> <em>New</em> The number of these tasks that were
   *               taken from either the entry queue or from other
   *               thread queues; that is, the number of tasks run
   *               that were <em>not</em> forked by the thread itself.
   *       <li> <em>Scan</em> The number of times other task
   *               queues or the entry queue were polled for tasks.
   *     </ul>
   *   <li> <em>Execute</em> The total number of tasks entered
   *        (but not necessarily yet run) via execute or invoke.
   *   <li> <em>Time</em> Time in seconds since construction of this
   *         FJTaskRunnerGroup.
   *   <li> <em>Rate</em> The total number of tasks processed
   *          per second across all threads. This
   *          may be useful as a simple throughput indicator
   *          if all processed tasks take approximately the
   *          same time to run.
   * </ul>
   * <p>
   * Cautions: Some statistics are updated and gathered
   * without synchronization,
   * so may not be accurate. However, reported counts may be considered
   * as lower bounds of actual values.
   * Some values may be zero if classes are compiled
   * with COLLECT_STATS set to false. (FJTaskRunner and FJTaskRunnerGroup
   * classes can be independently compiled with different values of
   * COLLECT_STATS.) Also, the counts are maintained as ints so could
   * overflow in exceptionally long-lived applications.
   * <p>
   * These statistics can be useful when tuning algorithms or diagnosing
   * problems. For example:
   * <ul>
   *  <li> High numbers of scans may mean that there is insufficient
   *      parallelism to keep threads busy. However, high scan rates
   *      are expected if the number
   *      of Executes is also high or there is a lot of global
   *      synchronization in the application, and the system is not otherwise
   *      busy. Threads may scan
   *      for work hundreds of times upon startup, shutdown, and
   *      global synch points of task sets.
   *  <li> Large imbalances in tasks run across different threads might
   *      just reflect contention with unrelated threads on a system
   *      (possibly including JVM threads such as GC), but may also
   *      indicate some systematic bias in how you generate tasks.
   *  <li> Large task queue capacities may mean that too many tasks are being
   *     generated before they can be run.
   *     Capacities are reported rather than current numbers of tasks
   *     in queues because they are better indicators of the existence
   *     of these kinds of possibly-transient problems.
   *     Queue capacities are
   *     resized on demand from their initial value of 4096 elements,
   *     which is much more than sufficient for the kinds of
   *     applications that this framework is intended to best support.
   * </ul>
   **/

  public void stats() {
    long time = System.currentTimeMillis() - initTime;
    double secs = ((double)time) / 1000.0;
    long totalRuns = 0;
    long totalScans = 0;
    long totalSteals = 0;

    System.out.print("Thread" +
                     "\tQ Cap" +
                       "\tScans" +
                       "\tNew" +
                       "\tRuns" +
                       "\n");

    for (int i = 0; i < threads.length; ++i) {
      FJTaskRunner t = threads[i];
      int truns = t.runs;
      totalRuns += truns;

      int tscans = t.scans;
      totalScans += tscans;

      int tsteals = t.steals;
      totalSteals += tsteals;

      String star = (getActive(t))? "*" : " ";


      System.out.print("T" + i + star +
                       "\t" + t.deqSize() +
                       "\t" + tscans +
                       "\t" + tsteals +
                       "\t" + truns +
                       "\n");
    }

    System.out.print("Total" +
                     "\t    " +
                     "\t" + totalScans +
                     "\t" + totalSteals +
                     "\t" + totalRuns +
                     "\n");

    System.out.print("Execute: " + entries);

    System.out.print("\tTime: " + secs);

    long rps = 0;
    if (secs != 0) rps = Math.round((double)(totalRuns) / secs);

    System.out.println("\tRate: " + rps);
  }


  /* ------------ Methods called only by FJTaskRunners ------------- */


  /**
   * Return the array of threads in this group.
   * Called only by FJTaskRunner.scan().
   **/

  public FJTaskRunner[] getArray() { return threads; }


  /**
   * Return a task from entry queue, or null if empty.
   * Called only by FJTaskRunner.scan().
   **/

  public FJTask pollEntryQueue() {
    try {
      FJTask t = (FJTask)(entryQueue.poll(0));
      return t;
    }
    catch(InterruptedException ex) { // ignore interrupts
      Thread.currentThread().interrupt();
      return null;
    }
  }


  /**
   * Return active status of t.
   * Per-thread active status can only be accessed and
   * modified via synchronized method here in the group class.
   **/

  protected synchronized boolean getActive(FJTaskRunner t) {
    return t.active;
  }


  /**
   * Set active status of thread t to true, and notify others
   * that might be waiting for work.
   **/

  public synchronized void setActive(FJTaskRunner t) {
    if (!t.active) {
      t.active = true;
      ++activeCount;
      if (nstarted < threads.length)
        threads[nstarted++].start();
      else
        notifyAll();
    }
  }

  /**
   * Set active status of thread t to false.
   **/

  public synchronized void setInactive(FJTaskRunner t) {
    if (t.active) {
      t.active = false;
      --activeCount;
    }
  }

  /**
   * The number of times to scan other threads for tasks
   * before transitioning to a mode where scans are
   * interleaved with sleeps (actually timed waits).
   * Upon transition, sleeps are for duration of
   * scans / SCANS_PER_SLEEP milliseconds.
   * <p>
   * This is not treated as a user-tunable parameter because
   * good values do not appear to vary much across JVMs or
   * applications. Its main role is to help avoid some
   * useless spinning and contention during task startup.
   **/
  static final long SCANS_PER_SLEEP = 15;

  /**
   * The maximum time (in msecs) to sleep when a thread is idle,
   * yet others are not, so may eventually generate work that
   * the current thread can steal. This value reflects the maximum time
   * that a thread may sleep when it possibly should not, because there
   * are other active threads that might generate work. In practice,
   * designs in which some threads become stalled because others
   * are running yet not generating tasks are not likely to work
   * well in this framework anyway, so the exact value does not matter
   * too much. However, keeping it in the sub-second range does
   * help smooth out startup and shutdown effects.
   **/

  static final long MAX_SLEEP_TIME = 100;

  /**
   * Set active status of thread t to false, and
   * then wait until: (a) there is a task in the entry
   * queue, or (b) other threads are active, or (c) the current
   * thread is interrupted. Upon return, it
   * is not certain that there will be work available.
   * The thread must itself check.
   * <p>
   * The main underlying reason
   * for these mechanics is that threads do not
   * signal each other when they add elements to their queues.
   * (This would add to task overhead, reduce locality.
   * and increase contention.)
   * So we must rely on a tamed form of polling. However, tasks
   * inserted into the entry queue do result in signals, so
   * tasks can wait on these if all of them are otherwise idle.
   **/

  public synchronized void checkActive(FJTaskRunner t, long scans) {

    setInactive(t);

    try {
      // if nothing available, do a hard wait
      if (activeCount == 0 && entryQueue.peek() == null) {
        wait();
      }
      else {
        // If there is possibly some work,
        // sleep for a while before rechecking

        long msecs = scans / SCANS_PER_SLEEP;
        if (msecs > MAX_SLEEP_TIME) msecs = MAX_SLEEP_TIME;
        int nsecs = (msecs == 0) ? 1 : 0; // forces shortest possible sleep
        wait(msecs, nsecs);
      }
    }
    catch (InterruptedException ex) {
      notify(); // avoid lost notifies on interrupts
      Thread.currentThread().interrupt();
    }
  }

  /* ------------ Utility methods  ------------- */

  /**
   * Start or wake up any threads waiting for work
   **/

  protected synchronized void signalNewTask() {
    if (COLLECT_STATS) ++entries;
    if (nstarted < threads.length)
       threads[nstarted++].start();
    else
      notify();
  }

  /**
   * Create all FJTaskRunner threads in this group.
   **/

  protected void initializeThreads() {
    for (int i = 0; i < threads.length; ++i) threads[i] = new FJTaskRunner(this);
  }




  /**
   * Wrap wait/notify mechanics around a task so that
   * invoke() can wait it out
   **/
  protected static final class InvokableFJTask extends FJTask {
    protected final Runnable wrapped;
    protected boolean terminated = false;

    protected InvokableFJTask(Runnable r) { wrapped = r; }

    public void run() {
      try {
        if (wrapped instanceof FJTask)
          FJTask.invoke((FJTask)(wrapped));
        else
          wrapped.run();
      }
      finally {
        setTerminated();
      }
    }

    protected synchronized void setTerminated() {
      terminated = true;
      notifyAll();
    }

    protected synchronized void awaitTermination() throws InterruptedException {
      while (!terminated) wait();
    }
  }


}

