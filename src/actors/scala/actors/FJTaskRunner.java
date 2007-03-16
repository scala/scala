/*
  File: FJTaskRunner.java

  Originally written by Doug Lea and released into the public domain.
  This may be used for any purposes whatsoever without acknowledgment.
  Thanks for the assistance and support of Sun Microsystems Labs,
  and everyone contributing, testing, and using this code.

  History:
  Date       Who                What
  7Jan1999   dl                 First public release
  13Jan1999  dl                 correct a stat counter update;
                                ensure inactive status on run termination;
                                misc minor cleaup
  14Jan1999  dl                 Use random starting point in scan;
                                variable renamings.
  18Jan1999  dl                 Runloop allowed to die on task exception;
                                remove useless timed join
  22Jan1999  dl                 Rework scan to allow use of priorities.
  6Feb1999   dl                 Documentation updates.
  7Mar1999   dl                 Add array-based coInvoke
  31Mar1999  dl                 Revise scan to remove need for NullTasks
  27Apr1999  dl                 Renamed
  23oct1999  dl                 Earlier detect of interrupt in scanWhileIdling
  24nov1999  dl                 Now works on JVMs that do not properly
                                implement read-after-write of 2 volatiles.
*/

package scala.actors;

import java.util.Random;

/**
 * Specialized Thread subclass for running FJTasks.
 * <p>
 * Each FJTaskRunner keeps FJTasks in a double-ended queue (DEQ).
 * Double-ended queues support stack-based operations
 * push and pop, as well as queue-based operations put and take.
 * Normally, threads run their own tasks. But they
 * may also steal tasks from each others DEQs.
 * <p>
 * The algorithms are minor variants of those used
 * in <A href="http://supertech.lcs.mit.edu/cilk/"> Cilk</A> and
 * <A href="http://www.cs.utexas.edu/users/hood/"> Hood</A>, and
 * to a lesser extent
 * <A href="http://www.cs.uga.edu/~dkl/filaments/dist.html"> Filaments</A>,
 * but are adapted to work in Java.
 * <p>
 * The two most important capabilities are:
 * <ul>
 *  <li> Fork a FJTask:
 *  <pre>
 *  Push task onto DEQ
 *  </pre>
 *  <li> Get a task to run (for example within taskYield)
 *  <pre>
 *  If DEQ is not empty,
 *     Pop a task and run it.
 *  Else if any other DEQ is not empty,
 *     Take ("steal") a task from it and run it.
 *  Else if the entry queue for our group is not empty,
 *     Take a task from it and run it.
 *  Else if current thread is otherwise idling
 *     If all threads are idling
 *        Wait for a task to be put on group entry queue
 *  Else
 *      Yield or Sleep for a while, and then retry
 *  </pre>
 * </ul>
 * The push, pop, and put are designed to only ever called by the
 * current thread, and take (steal) is only ever called by
 * other threads.
 * All other operations are composites and variants of these,
 * plus a few miscellaneous bookkeeping methods.
 * <p>
 * Implementations of the underlying representations and operations
 * are geared for use on JVMs operating on multiple CPUs (although
 * they should of course work fine on single CPUs as well).
 * <p>
 * A possible snapshot of a FJTaskRunner's DEQ is:
 * <pre>
 *     0     1     2     3     4     5     6    ...
 *  +-----+-----+-----+-----+-----+-----+-----+--
 *  |     |  t  |  t  |  t  |  t  |     |     | ...  deq array
 *  +-----+-----+-----+-----+-----+-----+-----+--
 *           ^                       ^
 *          base                    top
 *   (incremented                     (incremented
 *       on take,                         on push
 *    decremented                     decremented
 *       on put)                          on pop)
 * </pre>
 * <p>
 * FJTasks are held in elements of the DEQ.
 * They are maintained in a bounded array that
 * works similarly to a circular bounded buffer. To ensure
 * visibility of stolen FJTasks across threads, the array elements
 * must be <code>volatile</code>.
 * Using volatile rather than synchronizing suffices here since
 * each task accessed by a thread is either one that it
 * created or one that has never seen before. Thus we cannot
 * encounter any staleness problems executing run methods,
 * although FJTask programmers must be still sure to either synch or use
 * volatile for shared data within their run methods.
 * <p>
 * However, since there is no way
 * to declare an array of volatiles in Java, the DEQ elements actually
 * hold VolatileTaskRef objects, each of which in turn holds a
 * volatile reference to a FJTask.
 * Even with the double-indirection overhead of
 * volatile refs, using an array for the DEQ works out
 * better than linking them since fewer shared
 * memory locations need to be
 * touched or modified by the threads while using the DEQ.
 * Further, the double indirection may alleviate cache-line
 * sharing effects (which cannot otherwise be directly dealt with in Java).
 * <p>
 * The indices for the <code>base</code> and <code>top</code> of the DEQ
 * are declared as volatile. The main contention point with
 * multiple FJTaskRunner threads occurs when one thread is trying
 * to pop its own stack while another is trying to steal from it.
 * This is handled via a specialization of Dekker's algorithm,
 * in which the popping thread pre-decrements <code>top</code>,
 * and then checks it against <code>base</code>.
 * To be conservative in the face of JVMs that only partially
 * honor the specification for volatile, the pop proceeds
 * without synchronization only if there are apparently enough
 * items for both a simultaneous pop and take to succeed.
 * It otherwise enters a
 * synchronized lock to check if the DEQ is actually empty,
 * if so failing. The stealing thread
 * does almost the opposite, but is set up to be less likely
 * to win in cases of contention: Steals always run under synchronized
 * locks in order to avoid conflicts with other ongoing steals.
 * They pre-increment <code>base</code>, and then check against
 * <code>top</code>. They back out (resetting the base index
 * and failing to steal) if the
 * DEQ is empty or is about to become empty by an ongoing pop.
 * <p>
 * A push operation can normally run concurrently with a steal.
 * A push enters a synch lock only if the DEQ appears full so must
 * either be resized or have indices adjusted due to wrap-around
 * of the bounded DEQ. The put operation always requires synchronization.
 * <p>
 * When a FJTaskRunner thread has no tasks of its own to run,
 * it tries to be a good citizen.
 * Threads run at lower priority while scanning for work.
 * <p>
 * If the task is currently waiting
 * via yield, the thread alternates scans (starting at a randomly
 * chosen victim) with Thread.yields. This is
 * well-behaved so long as the JVM handles Thread.yield in a
 * sensible fashion. (It need not. Thread.yield is so underspecified
 * that it is legal for a JVM to treat it as a no-op.) This also
 * keeps things well-behaved even if we are running on a uniprocessor
 * JVM using a simple cooperative threading model.
 * <p>
 * If a thread needing work is
 * is otherwise idle (which occurs only in the main runloop), and
 * there are no available tasks to steal or poll, it
 * instead enters into a sleep-based (actually timed wait(msec))
 * phase in which it progressively sleeps for longer durations
 * (up to a maximum of FJTaskRunnerGroup.MAX_SLEEP_TIME,
 * currently 100ms) between scans.
 * If all threads in the group
 * are idling, they further progress to a hard wait phase, suspending
 * until a new task is entered into the FJTaskRunnerGroup entry queue.
 * A sleeping FJTaskRunner thread may be awakened by a new
 * task being put into the group entry queue or by another FJTaskRunner
 * becoming active, but not merely by some DEQ becoming non-empty.
 * Thus the MAX_SLEEP_TIME provides a bound for sleep durations
 * in cases where all but one worker thread start sleeping
 * even though there will eventually be work produced
 * by a thread that is taking a long time to place tasks in DEQ.
 * These sleep mechanics are handled in the FJTaskRunnerGroup class.
 * <p>
 * Composite operations such as taskJoin include heavy
 * manual inlining of the most time-critical operations
 * (mainly FJTask.invoke).
 * This opens up a few opportunities for further hand-optimizations.
 * Until Java compilers get a lot smarter, these tweaks
 * improve performance significantly enough for task-intensive
 * programs to be worth the poorer maintainability and code duplication.
 * <p>
 * Because they are so fragile and performance-sensitive, nearly
 * all methods are declared as final. However, nearly all fields
 * and methods are also declared as protected, so it is possible,
 * with much care, to extend functionality in subclasses. (Normally
 * you would also need to subclass FJTaskRunnerGroup.)
 * <p>
 * None of the normal java.lang.Thread class methods should ever be called
 * on FJTaskRunners. For this reason, it might have been nicer to
 * declare FJTaskRunner as a Runnable to run within a Thread. However,
 * this would have complicated many minor logistics. And since
 * no FJTaskRunner methods should normally be called from outside the
 * FJTask and FJTaskRunnerGroup classes either, this decision doesn't impact
 * usage.
 * <p>
 * You might think that layering this kind of framework on top of
 * Java threads, which are already several levels removed from raw CPU
 * scheduling on most systems, would lead to very poor performance.
 * But on the platforms
 * tested, the performance is quite good.
 * <p>[<a href="http://gee.cs.oswego.edu/dl/classes/EDU/oswego/cs/dl/util/concurrent/intro.html"> Introduction to this package. </a>]
 * @see FJTask
 * @see FJTaskRunnerGroup
 **/

public class FJTaskRunner extends Thread {

  /** The group of which this FJTaskRunner is a member **/
  protected final IFJTaskRunnerGroup group;

  /**
   *  Constructor called only during FJTaskRunnerGroup initialization
   **/

    /*protected*/ public FJTaskRunner(IFJTaskRunnerGroup g) {
    group = g;
    victimRNG = new Random(System.identityHashCode(this));
    runPriority = getPriority();
    setDaemon(true);
  }

  /**
   * Return the FJTaskRunnerGroup of which this thread is a member
   **/

  protected final IFJTaskRunnerGroup getGroup() { return group; }


  /* ------------ DEQ Representation ------------------- */


  /**
   * FJTasks are held in an array-based DEQ with INITIAL_CAPACITY
   * elements. The DEQ is grown if necessary, but default value is
   * normally much more than sufficient unless  there are
   * user programming errors or questionable operations generating
   * large numbers of Tasks without running them.
   * Capacities must be a power of two.
   **/

  protected static final int INITIAL_CAPACITY = 4096;

  /**
   * The maximum supported DEQ capacity.
   * When exceeded, FJTaskRunner operations throw Errors
   **/

  protected static final int MAX_CAPACITY = 1 << 30;

  /**
   * An object holding a single volatile reference to a FJTask.
   **/

  protected final static class VolatileTaskRef {
    /** The reference **/
    protected volatile FJTask ref;

    /** Set the reference **/
    protected final void put(FJTask r) { ref = r; }
    /** Return the reference **/
    protected final FJTask get()     { return ref; }
    /** Return the reference and clear it **/
    protected final FJTask take()    { FJTask r = ref; ref = null; return r;  }

    /**
     * Initialization utility for constructing arrays.
     * Make an array of given capacity and fill it with
     * VolatileTaskRefs.
     **/
    protected static VolatileTaskRef[] newArray(int cap) {
      VolatileTaskRef[] a = new VolatileTaskRef[cap];
      for (int k = 0; k < cap; k++) a[k] = new VolatileTaskRef();
      return a;
    }

  }

  /**
   * The DEQ array.
   **/

  protected VolatileTaskRef[] deq = VolatileTaskRef.newArray(INITIAL_CAPACITY);

  /** Current size of the task DEQ **/
  protected int deqSize() { return deq.length; }

  /**
   * Current top of DEQ. Generally acts just like a stack pointer in an
   * array-based stack, except that it circularly wraps around the
   * array, as in an array-based queue. The value is NOT
   * always kept within <code>0 ... deq.length</code> though.
   * The current top element is always at <code>top & (deq.length-1)</code>.
   * To avoid integer overflow, top is reset down
   * within bounds whenever it is noticed to be out out bounds;
   * at worst when it is at <code>2 * deq.length</code>.
   **/
  protected volatile int top = 0;


  /**
   * Current base of DEQ. Acts like a take-pointer in an
   * array-based bounded queue. Same bounds and usage as top.
   **/

  protected volatile int base = 0;


  /**
   * An extra object to synchronize on in order to
   * achieve a memory barrier.
   **/

  protected final Object barrier = new Object();

  /* ------------ Other BookKeeping ------------------- */

  /**
   * Record whether current thread may be processing a task
   * (i.e., has been started and is not in an idle wait).
   * Accessed, under synch, ONLY by FJTaskRunnerGroup, but the field is
   * stored here for simplicity.
   **/

    /*protected*/ public boolean active = false;

  /** Random starting point generator for scan() **/
  protected final Random victimRNG;


  /** Priority to use while scanning for work **/
    protected int scanPriority = Thread.MIN_PRIORITY + 1;

  /** Priority to use while running tasks **/
  protected int runPriority;

  /**
   * Set the priority to use while scanning.
   * We do not bother synchronizing access, since
   * by the time the value is needed, both this FJTaskRunner
   * and its FJTaskRunnerGroup will
   * necessarily have performed enough synchronization
   * to avoid staleness problems of any consequence.
   **/
  protected void setScanPriority(int pri) { scanPriority = pri; }


  /**
   * Set the priority to use while running tasks.
   * Same usage and rationale as setScanPriority.
   **/
  protected void setRunPriority(int pri) {  runPriority = pri; }

  /**
   * Compile-time constant for statistics gathering.
   * Even when set, reported values may not be accurate
   * since all are read and written without synchronization.
   **/



  static final boolean COLLECT_STATS = true;
  // static final boolean COLLECT_STATS = false;


  // for stat collection

  /** Total number of tasks run **/
  protected int runs = 0;

  /** Total number of queues scanned for work **/
  protected int scans = 0;

  /** Total number of tasks obtained via scan **/
  protected int steals = 0;



    /* -------- Suspending -------- */
    protected boolean suspending = false;

    synchronized void setSuspending(boolean susp) {
        suspending = susp;
    }

  /* ------------ DEQ operations ------------------- */


  /**
   * Push a task onto DEQ.
   * Called ONLY by current thread.
   **/

    /*protected*/ public final void push(final FJTask r) {
    int t = top;

    /*
      This test catches both overflows and index wraps.  It doesn't
      really matter if base value is in the midst of changing in take.
      As long as deq length is < 2^30, we are guaranteed to catch wrap in
      time since base can only be incremented at most length times
      between pushes (or puts).
    */

    if (t < (base & (deq.length-1)) + deq.length) {

      deq[t & (deq.length-1)].put(r);
      top = t + 1;
    }

    else  // isolate slow case to increase chances push is inlined
      slowPush(r); // check overflow and retry
  }


  /**
   * Handle slow case for push
   **/

  protected synchronized void slowPush(final FJTask r) {
    checkOverflow();
    push(r); // just recurse -- this one is sure to succeed.
  }


  /**
   * Enqueue task at base of DEQ.
   * Called ONLY by current thread.
   * This method is currently not called from class FJTask. It could be used
   * as a faster way to do FJTask.start, but most users would
   * find the semantics too confusing and unpredictable.
   **/

  protected final synchronized void put(final FJTask r) {
    for (;;) {
      int b = base - 1;
      if (top < b + deq.length) {

        int newBase = b & (deq.length-1);
        deq[newBase].put(r);
        base = newBase;

        if (b != newBase) { // Adjust for index underflow
          int newTop = top & (deq.length-1);
          if (newTop < newBase) newTop += deq.length;
          top = newTop;
        }
        return;
      }
      else {
        checkOverflow();
        // ... and retry
      }
    }
  }

  /**
   * Return a popped task, or null if DEQ is empty.
   * Called ONLY by current thread.
   * <p>
   * This is not usually called directly but is
   * instead inlined in callers. This version differs from the
   * cilk algorithm in that pop does not fully back down and
   * retry in the case of potential conflict with take. It simply
   * rechecks under synch lock. This gives a preference
   * for threads to run their own tasks, which seems to
   * reduce flailing a bit when there are few tasks to run.
   **/

  protected final FJTask pop() {
    /*
       Decrement top, to force a contending take to back down.
    */

    int t = --top;

    /*
      To avoid problems with JVMs that do not properly implement
      read-after-write of a pair of volatiles, we conservatively
      grab without lock only if the DEQ appears to have at least two
      elements, thus guaranteeing that both a pop and take will succeed,
      even if the pre-increment in take is not seen by current thread.
      Otherwise we recheck under synch.
    */

    if (base + 1 < t)
      return deq[t & (deq.length-1)].take();
    else
      return confirmPop(t);

  }


  /**
   * Check under synch lock if DEQ is really empty when doing pop.
   * Return task if not empty, else null.
   **/

  protected final synchronized FJTask confirmPop(int provisionalTop) {
    if (base <= provisionalTop)
      return deq[provisionalTop & (deq.length-1)].take();
    else {    // was empty
      /*
        Reset DEQ indices to zero whenever it is empty.
        This both avoids unnecessary calls to checkOverflow
        in push, and helps keep the DEQ from accumulating garbage
      */

      top = base = 0;
      return null;
    }
  }


  /**
   * Take a task from the base of the DEQ.
   * Always called by other threads via scan()
   **/


  protected final synchronized FJTask take() {

    /*
      Increment base in order to suppress a contending pop
    */

    int b = base++;

    if (b < top)
      return confirmTake(b);
    else {
      // back out
      base = b;
      return null;
    }
  }


  /**
   * double-check a potential take
   **/

  protected FJTask confirmTake(int oldBase) {

    /*
      Use a second (guaranteed uncontended) synch
      to serve as a barrier in case JVM does not
      properly process read-after-write of 2 volatiles
    */

    synchronized(barrier) {
      if (oldBase < top) {
        /*
          We cannot call deq[oldBase].take here because of possible races when
          nulling out versus concurrent push operations.  Resulting
          accumulated garbage is swept out periodically in
          checkOverflow, or more typically, just by keeping indices
          zero-based when found to be empty in pop, which keeps active
          region small and constantly overwritten.
        */

        return deq[oldBase & (deq.length-1)].get();
      }
      else {
        base = oldBase;
        return null;
      }
    }
  }


  /**
   * Adjust top and base, and grow DEQ if necessary.
   * Called only while DEQ synch lock being held.
   * We don't expect this to be called very often. In most
   * programs using FJTasks, it is never called.
   **/

  protected void checkOverflow() {
    int t = top;
    int b = base;

    if (t - b < deq.length-1) { // check if just need an index reset

      int newBase = b & (deq.length-1);
      int newTop  = top & (deq.length-1);
      if (newTop < newBase) newTop += deq.length;
      top = newTop;
      base = newBase;

      /*
         Null out refs to stolen tasks.
         This is the only time we can safely do it.
      */

      int i = newBase;
      while (i != newTop && deq[i].ref != null) {
        deq[i].ref = null;
        i = (i - 1) & (deq.length-1);
      }

    }
    else { // grow by doubling array

      int newTop = t - b;
      int oldcap = deq.length;
      int newcap = oldcap * 2;

      if (newcap >= MAX_CAPACITY)
        throw new Error("FJTask queue maximum capacity exceeded");

      VolatileTaskRef[] newdeq = new VolatileTaskRef[newcap];

      // copy in bottom half of new deq with refs from old deq
      for (int j = 0; j < oldcap; ++j) newdeq[j] = deq[b++ & (oldcap-1)];

      // fill top half of new deq with new refs
      for (int j = oldcap; j < newcap; ++j) newdeq[j] = new VolatileTaskRef();

      deq = newdeq;
      base = 0;
      top = newTop;
    }
  }


  /* ------------ Scheduling  ------------------- */


  /**
   * Do all but the pop() part of yield or join, by
   * traversing all DEQs in our group looking for a task to
   * steal. If none, it checks the entry queue.
   * <p>
   * Since there are no good, portable alternatives,
   * we rely here on a mixture of Thread.yield and priorities
   * to reduce wasted spinning, even though these are
   * not well defined. We are hoping here that the JVM
   * does something sensible.
   * @param waitingFor if non-null, the current task being joined
   **/

  protected void scan(final FJTask waitingFor) {

    FJTask task = null;

    // to delay lowering priority until first failure to steal
    boolean lowered = false;

    /*
      Circularly traverse from a random start index.

      This differs slightly from cilk version that uses a random index
      for each attempted steal.
      Exhaustive scanning might impede analytic tractablity of
      the scheduling policy, but makes it much easier to deal with
      startup and shutdown.
    */

    FJTaskRunner[] ts = group.getArray();
    int idx = victimRNG.nextInt(ts.length);

    for (int i = 0; i < ts.length; ++i) {

      FJTaskRunner t = ts[idx];
      if (++idx >= ts.length) idx = 0; // circularly traverse

      if (t != null && t != this) {

        if (waitingFor != null && waitingFor.isDone()) {
          break;
        }
        else {
          if (COLLECT_STATS) ++scans;
          task = t.take();
          if (task != null) {
            if (COLLECT_STATS) ++steals;
            break;
          }
          else if (isInterrupted()) {
            break;
          }
          else if (!lowered) { // if this is first fail, lower priority
            lowered = true;
            setPriority(scanPriority);
          }
          else {           // otherwise we are at low priority; just yield
            yield();
          }
        }
      }

    }

    if (task == null) {
      if (COLLECT_STATS) ++scans;
      task = group.pollEntryQueue();
      if (COLLECT_STATS) if (task != null) ++steals;
    }

    if (lowered) setPriority(runPriority);

    if (task != null && !task.isDone()) {
      if (COLLECT_STATS) ++runs;
      task.run();
      task.setDone();
    }

  }

  /**
   * Same as scan, but called when current thread is idling.
   * It repeatedly scans other threads for tasks,
   * sleeping while none are available.
   * <p>
   * This differs from scan mainly in that
   * since there is no reason to return to recheck any
   * condition, we iterate until a task is found, backing
   * off via sleeps if necessary.
   **/

  protected void scanWhileIdling() {
    FJTask task = null;

    boolean lowered = false;
    long iters = 0;

    FJTaskRunner[] ts = group.getArray();
    int idx = victimRNG.nextInt(ts.length);

    do {
      for (int i = 0; i < ts.length; ++i) {

        FJTaskRunner t = ts[idx];
        if (++idx >= ts.length) idx = 0; // circularly traverse

        if (t != null && t != this) {
          if (COLLECT_STATS) ++scans;

          task = t.take();
          if (task != null) {
            if (COLLECT_STATS) ++steals;
            if (lowered) setPriority(runPriority);
            group.setActive(this);
            break;
          }
        }
      }

      if (task == null) {
        if (isInterrupted())
          return;

        if (COLLECT_STATS) ++scans;
        task = group.pollEntryQueue();

        if (task != null) {
          if (COLLECT_STATS) ++steals;
          if (lowered) setPriority(runPriority);
          group.setActive(this);
        }
        else {
          ++iters;
          //  Check here for yield vs sleep to avoid entering group synch lock
          if (iters >= /*group.SCANS_PER_SLEEP*/ 15) {
            group.checkActive(this, iters);
            if (isInterrupted())
              return;
          }
          else if (!lowered) {
            lowered = true;
            setPriority(scanPriority);
          }
          else {
            yield();
          }
        }
      }
    } while (task == null);


    if (!task.isDone()) {
      if (COLLECT_STATS) ++runs;
      task.run();
      task.setDone();
    }

  }

  /* ------------  composite operations ------------------- */


  /**
   * Main runloop
   **/

  public void run() {
    try{
      while (!interrupted()) {
        FJTask task = pop();
        if (task != null) {
          if (!task.isDone()) {
            // inline FJTask.invoke
            if (COLLECT_STATS) ++runs;
            task.run();
            task.setDone();
          }
        }
        else
          scanWhileIdling();
      }
      // check for suspending
      if (suspending) {
          synchronized(this) {
              // move all local tasks to group-wide entry queue
              for (int i = 0; i < deq.length; ++i) {
                  synchronized(group) {
                      try {
                          FJTask task = (FJTask)deq[i].take();
                          if (task != null)
                              group.getEntryQueue().put(task);
                      } catch (InterruptedException ie) {
                          System.err.println("Suspend: when transferring task to entryQueue: "+ie);
                      }
                  }
              }
          }
      }
    }
    finally {
      group.setInactive(this);
    }
  }

  /**
   * Execute a task in this thread. Generally called when current task
   * cannot otherwise continue.
   **/


  protected final void taskYield() {
    FJTask task = pop();
    if (task != null) {
      if (!task.isDone()) {
        if (COLLECT_STATS) ++runs;
        task.run();
        task.setDone();
      }
    }
    else
      scan(null);
  }


  /**
   * Process tasks until w is done.
   * Equivalent to <code>while(!w.isDone()) taskYield(); </code>
   **/

  protected final void taskJoin(final FJTask w) {

    while (!w.isDone()) {

      FJTask task = pop();
      if (task != null) {
        if (!task.isDone()) {
          if (COLLECT_STATS) ++runs;
          task.run();
          task.setDone();
          if (task == w) return; // fast exit if we just ran w
        }
      }
      else
        scan(w);
    }
  }

  /**
   * A specialized expansion of
   * <code> w.fork(); invoke(v); w.join(); </code>
   **/


  protected final void coInvoke(final FJTask w, final FJTask v) {

    // inline  push

    int t = top;
    if (t < (base & (deq.length-1)) + deq.length) {

      deq[t & (deq.length-1)].put(w);
      top = t + 1;

      // inline  invoke

      if (!v.isDone()) {
        if (COLLECT_STATS) ++runs;
        v.run();
        v.setDone();
      }

      // inline  taskJoin

      while (!w.isDone()) {
        FJTask task  = pop();
        if (task != null) {
          if (!task.isDone()) {
            if (COLLECT_STATS) ++runs;
            task.run();
            task.setDone();
            if (task == w) return; // fast exit if we just ran w
          }
        }
        else
          scan(w);
      }
    }

    else      // handle non-inlinable cases
      slowCoInvoke(w, v);
  }


  /**
   * Backup to handle noninlinable cases of coInvoke
   **/

  protected void slowCoInvoke(final FJTask w, final FJTask v) {
    push(w); // let push deal with overflow
    FJTask.invoke(v);
    taskJoin(w);
  }


  /**
   * Array-based version of coInvoke
   **/

  protected final void coInvoke(FJTask[] tasks) {
    int nforks = tasks.length - 1;

    // inline bulk push of all but one task

    int t = top;

    if (nforks >= 0 && t + nforks < (base & (deq.length-1)) + deq.length) {
      for (int i = 0; i < nforks; ++i) {
        deq[t++ & (deq.length-1)].put(tasks[i]);
        top = t;
      }

      // inline invoke of one task
      FJTask v = tasks[nforks];
      if (!v.isDone()) {
        if (COLLECT_STATS) ++runs;
        v.run();
        v.setDone();
      }

      // inline  taskJoins

      for (int i = 0; i < nforks; ++i) {
        FJTask w = tasks[i];
        while (!w.isDone()) {

          FJTask task = pop();
          if (task != null) {
            if (!task.isDone()) {
              if (COLLECT_STATS) ++runs;
              task.run();
              task.setDone();
            }
          }
          else
            scan(w);
        }
      }
    }

    else  // handle non-inlinable cases
      slowCoInvoke(tasks);
  }

  /**
   * Backup to handle atypical or noninlinable cases of coInvoke
   **/

  protected void slowCoInvoke(FJTask[] tasks) {
    for (int i = 0; i < tasks.length; ++i) push(tasks[i]);
    for (int i = 0; i < tasks.length; ++i) taskJoin(tasks[i]);
  }

}

