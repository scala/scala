/*
 * Written by Doug Lea with assistance from members of JCP JSR-166
 * Expert Group and released to the public domain, as explained at
 * http://creativecommons.org/licenses/publicdomain
 */

package scala.actors.forkjoin;
import java.io.Serializable;
import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.*;
import sun.misc.Unsafe;
import java.lang.reflect.*;

/**
 * Abstract base class for tasks that run within a {@link
 * ForkJoinPool}.  A ForkJoinTask is a thread-like entity that is much
 * lighter weight than a normal thread.  Huge numbers of tasks and
 * subtasks may be hosted by a small number of actual threads in a
 * ForkJoinPool, at the price of some usage limitations.
 *
 * <p> A "main" ForkJoinTask begins execution when submitted to a
 * {@link ForkJoinPool}. Once started, it will usually in turn start
 * other subtasks.  As indicated by the name of this class, many
 * programs using ForkJoinTasks employ only methods <code>fork</code>
 * and <code>join</code>, or derivatives such as
 * <code>invokeAll</code>.  However, this class also provides a number
 * of other methods that can come into play in advanced usages, as
 * well as extension mechanics that allow support of new forms of
 * fork/join processing.
 *
 * <p>A ForkJoinTask is a lightweight form of {@link Future}.  The
 * efficiency of ForkJoinTasks stems from a set of restrictions (that
 * are only partially statically enforceable) reflecting their
 * intended use as computational tasks calculating pure functions or
 * operating on purely isolated objects.  The primary coordination
 * mechanisms are {@link #fork}, that arranges asynchronous execution,
 * and {@link #join}, that doesn't proceed until the task's result has
 * been computed.  Computations should avoid <code>synchronized</code>
 * methods or blocks, and should minimize other blocking
 * synchronization apart from joining other tasks or using
 * synchronizers such as Phasers that are advertised to cooperate with
 * fork/join scheduling. Tasks should also not perform blocking IO,
 * and should ideally access variables that are completely independent
 * of those accessed by other running tasks. Minor breaches of these
 * restrictions, for example using shared output streams, may be
 * tolerable in practice, but frequent use may result in poor
 * performance, and the potential to indefinitely stall if the number
 * of threads not waiting for IO or other external synchronization
 * becomes exhausted. This usage restriction is in part enforced by
 * not permitting checked exceptions such as <code>IOExceptions</code>
 * to be thrown. However, computations may still encounter unchecked
 * exceptions, that are rethrown to callers attempting join
 * them. These exceptions may additionally include
 * RejectedExecutionExceptions stemming from internal resource
 * exhaustion such as failure to allocate internal task queues.
 *
 * <p>The primary method for awaiting completion and extracting
 * results of a task is {@link #join}, but there are several variants:
 * The {@link Future#get} methods support interruptible and/or timed
 * waits for completion and report results using <code>Future</code>
 * conventions. Method {@link #helpJoin} enables callers to actively
 * execute other tasks while awaiting joins, which is sometimes more
 * efficient but only applies when all subtasks are known to be
 * strictly tree-structured. Method {@link #invoke} is semantically
 * equivalent to <code>fork(); join()</code> but always attempts to
 * begin execution in the current thread. The "<em>quiet</em>" forms
 * of these methods do not extract results or report exceptions. These
 * may be useful when a set of tasks are being executed, and you need
 * to delay processing of results or exceptions until all complete.
 * Method <code>invokeAll</code> (available in multiple versions)
 * performs the most common form of parallel invocation: forking a set
 * of tasks and joining them all.
 *
 * <p> The ForkJoinTask class is not usually directly subclassed.
 * Instead, you subclass one of the abstract classes that support a
 * particular style of fork/join processing.  Normally, a concrete
 * ForkJoinTask subclass declares fields comprising its parameters,
 * established in a constructor, and then defines a <code>compute</code>
 * method that somehow uses the control methods supplied by this base
 * class. While these methods have <code>public</code> access (to allow
 * instances of different task subclasses to call each others
 * methods), some of them may only be called from within other
 * ForkJoinTasks. Attempts to invoke them in other contexts result in
 * exceptions or errors possibly including ClassCastException.
 *
 * <p>Most base support methods are <code>final</code> because their
 * implementations are intrinsically tied to the underlying
 * lightweight task scheduling framework, and so cannot be overridden.
 * Developers creating new basic styles of fork/join processing should
 * minimally implement <code>protected</code> methods
 * <code>exec</code>, <code>setRawResult</code>, and
 * <code>getRawResult</code>, while also introducing an abstract
 * computational method that can be implemented in its subclasses,
 * possibly relying on other <code>protected</code> methods provided
 * by this class.
 *
 * <p>ForkJoinTasks should perform relatively small amounts of
 * computations, othewise splitting into smaller tasks. As a very
 * rough rule of thumb, a task should perform more than 100 and less
 * than 10000 basic computational steps. If tasks are too big, then
 * parellelism cannot improve throughput. If too small, then memory
 * and internal task maintenance overhead may overwhelm processing.
 *
 * <p>ForkJoinTasks are <code>Serializable</code>, which enables them
 * to be used in extensions such as remote execution frameworks. It is
 * in general sensible to serialize tasks only before or after, but
 * not during execution. Serialization is not relied on during
 * execution itself.
 */
public abstract class ForkJoinTask<V> implements Future<V>, Serializable {

    /**
     * Run control status bits packed into a single int to minimize
     * footprint and to ensure atomicity (via CAS).  Status is
     * initially zero, and takes on nonnegative values until
     * completed, upon which status holds COMPLETED. CANCELLED, or
     * EXCEPTIONAL, which use the top 3 bits.  Tasks undergoing
     * blocking waits by other threads have SIGNAL_MASK bits set --
     * bit 15 for external (nonFJ) waits, and the rest a count of
     * waiting FJ threads.  (This representation relies on
     * ForkJoinPool max thread limits). Completion of a stolen task
     * with SIGNAL_MASK bits set awakens waiter via notifyAll. Even
     * though suboptimal for some purposes, we use basic builtin
     * wait/notify to take advantage of "monitor inflation" in JVMs
     * that we would otherwise need to emulate to avoid adding further
     * per-task bookkeeping overhead. Note that bits 16-28 are
     * currently unused. Also value 0x80000000 is available as spare
     * completion value.
     */
    volatile int status; // accessed directy by pool and workers

    static final int COMPLETION_MASK      = 0xe0000000;
    static final int NORMAL               = 0xe0000000; // == mask
    static final int CANCELLED            = 0xc0000000;
    static final int EXCEPTIONAL          = 0xa0000000;
    static final int SIGNAL_MASK          = 0x0000ffff;
    static final int INTERNAL_SIGNAL_MASK = 0x00007fff;
    static final int EXTERNAL_SIGNAL      = 0x00008000; // top bit of low word

    /**
     * Table of exceptions thrown by tasks, to enable reporting by
     * callers. Because exceptions are rare, we don't directly keep
     * them with task objects, but instead us a weak ref table.  Note
     * that cancellation exceptions don't appear in the table, but are
     * instead recorded as status values.
     * Todo: Use ConcurrentReferenceHashMap
     */
    static final Map<ForkJoinTask<?>, Throwable> exceptionMap =
        Collections.synchronizedMap
        (new WeakHashMap<ForkJoinTask<?>, Throwable>());

    // within-package utilities

    /**
     * Get current worker thread, or null if not a worker thread
     */
    static ForkJoinWorkerThread getWorker() {
        Thread t = Thread.currentThread();
        return ((t instanceof ForkJoinWorkerThread)?
                (ForkJoinWorkerThread)t : null);
    }

    final boolean casStatus(int cmp, int val) {
        return _unsafe.compareAndSwapInt(this, statusOffset, cmp, val);
    }

    /**
     * Workaround for not being able to rethrow unchecked exceptions.
     */
    static void rethrowException(Throwable ex) {
        if (ex != null)
            _unsafe.throwException(ex);
    }

    // Setting completion status

    /**
     * Mark completion and wake up threads waiting to join this task.
     * @param completion one of NORMAL, CANCELLED, EXCEPTIONAL
     */
    final void setCompletion(int completion) {
        ForkJoinPool pool = getPool();
        if (pool != null) {
            int s; // Clear signal bits while setting completion status
            do;while ((s = status) >= 0 && !casStatus(s, completion));

            if ((s & SIGNAL_MASK) != 0) {
                if ((s &= INTERNAL_SIGNAL_MASK) != 0)
                    pool.updateRunningCount(s);
                synchronized(this) { notifyAll(); }
            }
        }
        else
            externallySetCompletion(completion);
    }

    /**
     * Version of setCompletion for non-FJ threads.  Leaves signal
     * bits for unblocked threads to adjust, and always notifies.
     */
    private void externallySetCompletion(int completion) {
        int s;
        do;while ((s = status) >= 0 &&
                  !casStatus(s, (s & SIGNAL_MASK) | completion));
        synchronized(this) { notifyAll(); }
    }

    /**
     * Sets status to indicate normal completion
     */
    final void setNormalCompletion() {
        // Try typical fast case -- single CAS, no signal, not already done.
        // Manually expand casStatus to improve chances of inlining it
        if (!_unsafe.compareAndSwapInt(this, statusOffset, 0, NORMAL))
            setCompletion(NORMAL);
    }

    // internal waiting and notification

    /**
     * Performs the actual monitor wait for awaitDone
     */
    private void doAwaitDone() {
        // Minimize lock bias and in/de-flation effects by maximizing
        // chances of waiting inside sync
        try {
            while (status >= 0)
                synchronized(this) { if (status >= 0) wait(); }
        } catch (InterruptedException ie) {
            onInterruptedWait();
        }
    }

    /**
     * Performs the actual monitor wait for awaitDone
     */
    private void doAwaitDone(long startTime, long nanos) {
        synchronized(this) {
            try {
                while (status >= 0) {
                    long nt = nanos - System.nanoTime() - startTime;
                    if (nt <= 0)
                        break;
                    wait(nt / 1000000, (int)(nt % 1000000));
                }
            } catch (InterruptedException ie) {
                onInterruptedWait();
            }
        }
    }

    // Awaiting completion

    /**
     * Sets status to indicate there is joiner, then waits for join,
     * surrounded with pool notifications.
     * @return status upon exit
     */
    private int awaitDone(ForkJoinWorkerThread w, boolean maintainParallelism) {
        ForkJoinPool pool = w == null? null : w.pool;
        int s;
        while ((s = status) >= 0) {
            if (casStatus(s, pool == null? s|EXTERNAL_SIGNAL : s+1)) {
                if (pool == null || !pool.preJoin(this, maintainParallelism))
                    doAwaitDone();
                if (((s = status) & INTERNAL_SIGNAL_MASK) != 0)
                    adjustPoolCountsOnUnblock(pool);
                break;
            }
        }
        return s;
    }

    /**
     * Timed version of awaitDone
     * @return status upon exit
     */
    private int awaitDone(ForkJoinWorkerThread w, long nanos) {
        ForkJoinPool pool = w == null? null : w.pool;
        int s;
        while ((s = status) >= 0) {
            if (casStatus(s, pool == null? s|EXTERNAL_SIGNAL : s+1)) {
                long startTime = System.nanoTime();
                if (pool == null || !pool.preJoin(this, false))
                    doAwaitDone(startTime, nanos);
                if ((s = status) >= 0) {
                    adjustPoolCountsOnCancelledWait(pool);
                    s = status;
                }
                if (s < 0 && (s & INTERNAL_SIGNAL_MASK) != 0)
                    adjustPoolCountsOnUnblock(pool);
                break;
            }
        }
        return s;
    }

    /**
     * Notify pool that thread is unblocked. Called by signalled
     * threads when woken by non-FJ threads (which is atypical).
     */
    private void adjustPoolCountsOnUnblock(ForkJoinPool pool) {
        int s;
        do;while ((s = status) < 0 && !casStatus(s, s & COMPLETION_MASK));
        if (pool != null && (s &= INTERNAL_SIGNAL_MASK) != 0)
            pool.updateRunningCount(s);
    }

    /**
     * Notify pool to adjust counts on cancelled or timed out wait
     */
    private void adjustPoolCountsOnCancelledWait(ForkJoinPool pool) {
        if (pool != null) {
            int s;
            while ((s = status) >= 0 && (s & INTERNAL_SIGNAL_MASK) != 0) {
                if (casStatus(s, s - 1)) {
                    pool.updateRunningCount(1);
                    break;
                }
            }
        }
    }

    /**
     * Handle interruptions during waits.
     */
    private void onInterruptedWait() {
        ForkJoinWorkerThread w = getWorker();
        if (w == null)
            Thread.currentThread().interrupt(); // re-interrupt
        else if (w.isTerminating())
            cancelIgnoringExceptions();
        // else if FJworker, ignore interrupt
    }

    // Recording and reporting exceptions

    private void setDoneExceptionally(Throwable rex) {
        exceptionMap.put(this, rex);
        setCompletion(EXCEPTIONAL);
    }

    /**
     * Throws the exception associated with status s;
     * @throws the exception
     */
    private void reportException(int s) {
        if ((s &= COMPLETION_MASK) < NORMAL) {
            if (s == CANCELLED)
                throw new CancellationException();
            else
                rethrowException(exceptionMap.get(this));
        }
    }

    /**
     * Returns result or throws exception using j.u.c.Future conventions
     * Only call when isDone known to be true.
     */
    private V reportFutureResult()
        throws ExecutionException, InterruptedException {
        int s = status & COMPLETION_MASK;
        if (s < NORMAL) {
            Throwable ex;
            if (s == CANCELLED)
                throw new CancellationException();
            if (s == EXCEPTIONAL && (ex = exceptionMap.get(this)) != null)
                throw new ExecutionException(ex);
            if (Thread.interrupted())
                throw new InterruptedException();
        }
        return getRawResult();
    }

    /**
     * Returns result or throws exception using j.u.c.Future conventions
     * with timeouts
     */
    private V reportTimedFutureResult()
        throws InterruptedException, ExecutionException, TimeoutException {
        Throwable ex;
        int s = status & COMPLETION_MASK;
        if (s == NORMAL)
            return getRawResult();
        if (s == CANCELLED)
            throw new CancellationException();
        if (s == EXCEPTIONAL && (ex = exceptionMap.get(this)) != null)
            throw new ExecutionException(ex);
        if (Thread.interrupted())
            throw new InterruptedException();
        throw new TimeoutException();
    }

    // internal execution methods

    /**
     * Calls exec, recording completion, and rethrowing exception if
     * encountered. Caller should normally check status before calling
     * @return true if completed normally
     */
    private boolean tryExec() {
        try { // try block must contain only call to exec
            if (!exec())
                return false;
        } catch (Throwable rex) {
            setDoneExceptionally(rex);
            rethrowException(rex);
            return false; // not reached
        }
        setNormalCompletion();
        return true;
    }

    /**
     * Main execution method used by worker threads. Invokes
     * base computation unless already complete
     */
    final void quietlyExec() {
        if (status >= 0) {
            try {
                if (!exec())
                    return;
            } catch(Throwable rex) {
                setDoneExceptionally(rex);
                return;
            }
            setNormalCompletion();
        }
    }

    /**
     * Calls exec, recording but not rethrowing exception
     * Caller should normally check status before calling
     * @return true if completed normally
     */
    private boolean tryQuietlyInvoke() {
        try {
            if (!exec())
                return false;
        } catch (Throwable rex) {
            setDoneExceptionally(rex);
            return false;
        }
        setNormalCompletion();
        return true;
    }

    /**
     * Cancel, ignoring any exceptions it throws
     */
    final void cancelIgnoringExceptions() {
        try {
            cancel(false);
        } catch(Throwable ignore) {
        }
    }

    /**
     * Main implementation of helpJoin
     */
    private int busyJoin(ForkJoinWorkerThread w) {
        int s;
        ForkJoinTask<?> t;
        while ((s = status) >= 0 && (t = w.scanWhileJoining(this)) != null)
            t.quietlyExec();
        return (s >= 0)? awaitDone(w, false) : s; // block if no work
    }

    // public methods

    /**
     * Arranges to asynchronously execute this task.  While it is not
     * necessarily enforced, it is a usage error to fork a task more
     * than once unless it has completed and been reinitialized.  This
     * method may be invoked only from within ForkJoinTask
     * computations. Attempts to invoke in other contexts result in
     * exceptions or errors possibly including ClassCastException.
     */
    public final void fork() {
        ((ForkJoinWorkerThread)(Thread.currentThread())).pushTask(this);
    }

    /**
     * Returns the result of the computation when it is ready.
     * This method differs from <code>get</code> in that abnormal
     * completion results in RuntimeExceptions or Errors, not
     * ExecutionExceptions.
     *
     * @return the computed result
     */
    public final V join() {
        ForkJoinWorkerThread w = getWorker();
        if (w == null || status < 0 || !w.unpushTask(this) || !tryExec())
            reportException(awaitDone(w, true));
        return getRawResult();
    }

    /**
     * Commences performing this task, awaits its completion if
     * necessary, and return its result.
     * @throws Throwable (a RuntimeException, Error, or unchecked
     * exception) if the underlying computation did so.
     * @return the computed result
     */
    public final V invoke() {
        if (status >= 0 && tryExec())
            return getRawResult();
        else
            return join();
    }

    /**
     * Forks both tasks, returning when <code>isDone</code> holds for
     * both of them or an exception is encountered. This method may be
     * invoked only from within ForkJoinTask computations. Attempts to
     * invoke in other contexts result in exceptions or errors
     * possibly including ClassCastException.
     * @param t1 one task
     * @param t2 the other task
     * @throws NullPointerException if t1 or t2 are null
     * @throws RuntimeException or Error if either task did so.
     */
    public static void invokeAll(ForkJoinTask<?>t1, ForkJoinTask<?> t2) {
        t2.fork();
        t1.invoke();
        t2.join();
    }

    /**
     * Forks the given tasks, returning when <code>isDone</code> holds
     * for all of them. If any task encounters an exception, others
     * may be cancelled.  This method may be invoked only from within
     * ForkJoinTask computations. Attempts to invoke in other contexts
     * result in exceptions or errors possibly including ClassCastException.
     * @param tasks the array of tasks
     * @throws NullPointerException if tasks or any element are null.
     * @throws RuntimeException or Error if any task did so.
     */
    public static void invokeAll(ForkJoinTask<?>... tasks) {
        Throwable ex = null;
        int last = tasks.length - 1;
        for (int i = last; i >= 0; --i) {
            ForkJoinTask<?> t = tasks[i];
            if (t == null) {
                if (ex == null)
                    ex = new NullPointerException();
            }
            else if (i != 0)
                t.fork();
            else {
                t.quietlyInvoke();
                if (ex == null)
                    ex = t.getException();
            }
        }
        for (int i = 1; i <= last; ++i) {
            ForkJoinTask<?> t = tasks[i];
            if (t != null) {
                if (ex != null)
                    t.cancel(false);
                else {
                    t.quietlyJoin();
                    if (ex == null)
                        ex = t.getException();
                }
            }
        }
        if (ex != null)
            rethrowException(ex);
    }

    /**
     * Forks all tasks in the collection, returning when
     * <code>isDone</code> holds for all of them. If any task
     * encounters an exception, others may be cancelled.  This method
     * may be invoked only from within ForkJoinTask
     * computations. Attempts to invoke in other contexts resul!t in
     * exceptions or errors possibly including ClassCastException.
     * @param tasks the collection of tasks
     * @throws NullPointerException if tasks or any element are null.
     * @throws RuntimeException or Error if any task did so.
     */
    public static void invokeAll(Collection<? extends ForkJoinTask<?>> tasks) {
        if (!(tasks instanceof List)) {
            invokeAll(tasks.toArray(new ForkJoinTask[tasks.size()]));
            return;
        }
        List<? extends ForkJoinTask<?>> ts =
            (List<? extends ForkJoinTask<?>>)tasks;
        Throwable ex = null;
        int last = ts.size() - 1;
        for (int i = last; i >= 0; --i) {
            ForkJoinTask<?> t = ts.get(i);
            if (t == null) {
                if (ex == null)
                    ex = new NullPointerException();
            }
            else if (i != 0)
                t.fork();
            else {
                t.quietlyInvoke();
                if (ex == null)
                    ex = t.getException();
            }
        }
        for (int i = 1; i <= last; ++i) {
            ForkJoinTask<?> t = ts.get(i);
            if (t != null) {
                if (ex != null)
                    t.cancel(false);
                else {
                    t.quietlyJoin();
                    if (ex == null)
                        ex = t.getException();
                }
            }
        }
        if (ex != null)
            rethrowException(ex);
    }

    /**
     * Returns true if the computation performed by this task has
     * completed (or has been cancelled).
     * @return true if this computation has completed
     */
    public final boolean isDone() {
        return status < 0;
    }

    /**
     * Returns true if this task was cancelled.
     * @return true if this task was cancelled
     */
    public final boolean isCancelled() {
        return (status & COMPLETION_MASK) == CANCELLED;
    }

    /**
     * Asserts that the results of this task's computation will not be
     * used. If a cancellation occurs before atempting to execute this
     * task, then execution will be suppressed, <code>isCancelled</code>
     * will report true, and <code>join</code> will result in a
     * <code>CancellationException</code> being thrown. Otherwise, when
     * cancellation races with completion, there are no guarantees
     * about whether <code>isCancelled</code> will report true, whether
     * <code>join</code> will return normally or via an exception, or
     * whether these behaviors will remain consistent upon repeated
     * invocation.
     *
     * <p>This method may be overridden in subclasses, but if so, must
     * still ensure that these minimal properties hold. In particular,
     * the cancel method itself must not throw exceptions.
     *
     * <p> This method is designed to be invoked by <em>other</em>
     * tasks. To terminate the current task, you can just return or
     * throw an unchecked exception from its computation method, or
     * invoke <code>completeExceptionally</code>.
     *
     * @param mayInterruptIfRunning this value is ignored in the
     * default implementation because tasks are not in general
     * cancelled via interruption.
     *
     * @return true if this task is now cancelled
     */
    public boolean cancel(boolean mayInterruptIfRunning) {
        setCompletion(CANCELLED);
        return (status & COMPLETION_MASK) == CANCELLED;
    }

    /**
     * Returns true if this task threw an exception or was cancelled
     * @return true if this task threw an exception or was cancelled
     */
    public final boolean isCompletedAbnormally() {
        return (status & COMPLETION_MASK) < NORMAL;
    }

    /**
     * Returns the exception thrown by the base computation, or a
     * CancellationException if cancelled, or null if none or if the
     * method has not yet completed.
     * @return the exception, or null if none
     */
    public final Throwable getException() {
        int s = status & COMPLETION_MASK;
        if (s >= NORMAL)
            return null;
        if (s == CANCELLED)
            return new CancellationException();
        return exceptionMap.get(this);
    }

    /**
     * Completes this task abnormally, and if not already aborted or
     * cancelled, causes it to throw the given exception upon
     * <code>join</code> and related operations. This method may be used
     * to induce exceptions in asynchronous tasks, or to force
     * completion of tasks that would not otherwise complete.  Its use
     * in other situations is likely to be wrong.  This method is
     * overridable, but overridden versions must invoke <code>super</code>
     * implementation to maintain guarantees.
     *
     * @param ex the exception to throw. If this exception is
     * not a RuntimeException or Error, the actual exception thrown
     * will be a RuntimeException with cause ex.
     */
    public void completeExceptionally(Throwable ex) {
        setDoneExceptionally((ex instanceof RuntimeException) ||
                             (ex instanceof Error)? ex :
                             new RuntimeException(ex));
    }

    /**
     * Completes this task, and if not already aborted or cancelled,
     * returning a <code>null</code> result upon <code>join</code> and related
     * operations. This method may be used to provide results for
     * asynchronous tasks, or to provide alternative handling for
     * tasks that would not otherwise complete normally. Its use in
     * other situations is likely to be wrong. This method is
     * overridable, but overridden versions must invoke <code>super</code>
     * implementation to maintain guarantees.
     *
     * @param value the result value for this task.
     */
    public void complete(V value) {
        try {
            setRawResult(value);
        } catch(Throwable rex) {
            setDoneExceptionally(rex);
            return;
        }
        setNormalCompletion();
    }

    public final V get() throws InterruptedException, ExecutionException {
        ForkJoinWorkerThread w = getWorker();
        if (w == null || status < 0 || !w.unpushTask(this) || !tryQuietlyInvoke())
            awaitDone(w, true);
        return reportFutureResult();
    }

    public final V get(long timeout, TimeUnit unit)
        throws InterruptedException, ExecutionException, TimeoutException {
        ForkJoinWorkerThread w = getWorker();
        if (w == null || status < 0 || !w.unpushTask(this) || !tryQuietlyInvoke())
            awaitDone(w, unit.toNanos(timeout));
        return reportTimedFutureResult();
    }

    /**
     * Possibly executes other tasks until this task is ready, then
     * returns the result of the computation.  This method may be more
     * efficient than <code>join</code>, but is only applicable when
     * there are no potemtial dependencies between continuation of the
     * current task and that of any other task that might be executed
     * while helping. (This usually holds for pure divide-and-conquer
     * tasks). This method may be invoked only from within
     * ForkJoinTask computations. Attempts to invoke in other contexts
     * resul!t in exceptions or errors possibly including ClassCastException.
     * @return the computed result
     */
    public final V helpJoin() {
        ForkJoinWorkerThread w = (ForkJoinWorkerThread)(Thread.currentThread());
        if (status < 0 || !w.unpushTask(this) || !tryExec())
            reportException(busyJoin(w));
        return getRawResult();
    }

    /**
     * Possibly executes other tasks until this task is ready.  This
     * method may be invoked only from within ForkJoinTask
     * computations. Attempts to invoke in other contexts resul!t in
     * exceptions or errors possibly including ClassCastException.
     */
    public final void quietlyHelpJoin() {
        if (status >= 0) {
            ForkJoinWorkerThread w =
                (ForkJoinWorkerThread)(Thread.currentThread());
            if (!w.unpushTask(this) || !tryQuietlyInvoke())
                busyJoin(w);
        }
    }

    /**
     * Joins this task, without returning its result or throwing an
     * exception. This method may be useful when processing
     * collections of tasks when some have been cancelled or otherwise
     * known to have aborted.
     */
    public final void quietlyJoin() {
        if (status >= 0) {
            ForkJoinWorkerThread w = getWorker();
            if (w == null || !w.unpushTask(this) || !tryQuietlyInvoke())
                awaitDone(w, true);
        }
    }

    /**
     * Commences performing this task and awaits its completion if
     * necessary, without returning its result or throwing an
     * exception. This method may be useful when processing
     * collections of tasks when some have been cancelled or otherwise
     * known to have aborted.
     */
    public final void quietlyInvoke() {
        if (status >= 0 && !tryQuietlyInvoke())
            quietlyJoin();
    }

    /**
     * Possibly executes tasks until the pool hosting the current task
     * {@link ForkJoinPool#isQuiescent}. This method may be of use in
     * designs in which many tasks are forked, but none are explicitly
     * joined, instead executing them until all are processed.
     */
    public static void helpQuiesce() {
        ((ForkJoinWorkerThread)(Thread.currentThread())).
            helpQuiescePool();
    }

    /**
     * Resets the internal bookkeeping state of this task, allowing a
     * subsequent <code>fork</code>. This method allows repeated reuse of
     * this task, but only if reuse occurs when this task has either
     * never been forked, or has been forked, then completed and all
     * outstanding joins of this task have also completed. Effects
     * under any other usage conditions are not guaranteed, and are
     * almost surely wrong. This method may be useful when executing
     * pre-constructed trees of subtasks in loops.
     */
    public void reinitialize() {
        if ((status & COMPLETION_MASK) == EXCEPTIONAL)
            exceptionMap.remove(this);
        status = 0;
    }

    /**
     * Returns the pool hosting the current task execution, or null
     * if this task is executing outside of any pool.
     * @return the pool, or null if none.
     */
    public static ForkJoinPool getPool() {
        Thread t = Thread.currentThread();
        return ((t instanceof ForkJoinWorkerThread)?
                ((ForkJoinWorkerThread)t).pool : null);
    }

    /**
     * Tries to unschedule this task for execution. This method will
     * typically succeed if this task is the most recently forked task
     * by the current thread, and has not commenced executing in
     * another thread.  This method may be useful when arranging
     * alternative local processing of tasks that could have been, but
     * were not, stolen. This method may be invoked only from within
     * ForkJoinTask computations. Attempts to invoke in other contexts
     * result in exceptions or errors possibly including ClassCastException.
     * @return true if unforked
     */
    public boolean tryUnfork() {
        return ((ForkJoinWorkerThread)(Thread.currentThread())).unpushTask(this);
    }

    /**
     * Returns an estimate of the number of tasks that have been
     * forked by the current worker thread but not yet executed. This
     * value may be useful for heuristic decisions about whether to
     * fork other tasks.
     * @return the number of tasks
     */
    public static int getQueuedTaskCount() {
        return ((ForkJoinWorkerThread)(Thread.currentThread())).
            getQueueSize();
    }

    /**
     * Returns a estimate of how many more locally queued tasks are
     * held by the current worker thread than there are other worker
     * threads that might steal them.  This value may be useful for
     * heuristic decisions about whether to fork other tasks. In many
     * usages of ForkJoinTasks, at steady state, each worker should
     * aim to maintain a small constant surplus (for example, 3) of
     * tasks, and to process computations locally if this threshold is
     * exceeded.
     * @return the surplus number of tasks, which may be negative
     */
    public static int getSurplusQueuedTaskCount() {
        return ((ForkJoinWorkerThread)(Thread.currentThread()))
            .getEstimatedSurplusTaskCount();
    }

    // Extension methods

    /**
     * Returns the result that would be returned by <code>join</code>,
     * even if this task completed abnormally, or null if this task is
     * not known to have been completed.  This method is designed to
     * aid debugging, as well as to support extensions. Its use in any
     * other context is discouraged.
     *
     * @return the result, or null if not completed.
     */
    public abstract V getRawResult();

    /**
     * Forces the given value to be returned as a result.  This method
     * is designed to support extensions, and should not in general be
     * called otherwise.
     *
     * @param value the value
     */
    protected abstract void setRawResult(V value);

    /**
     * Immediately performs the base action of this task.  This method
     * is designed to support extensions, and should not in general be
     * called otherwise. The return value controls whether this task
     * is considered to be done normally. It may return false in
     * asynchronous actions that require explicit invocations of
     * <code>complete</code> to become joinable. It may throw exceptions
     * to indicate abnormal exit.
     * @return true if completed normally
     * @throws Error or RuntimeException if encountered during computation
     */
    protected abstract boolean exec();

    /**
     * Returns, but does not unschedule or execute, the task queued by
     * the current thread but not yet executed, if one is
     * available. There is no guarantee that this task will actually
     * be polled or executed next.  This method is designed primarily
     * to support extensions, and is unlikely to be useful otherwise.
     * This method may be invoked only from within ForkJoinTask
     * computations. Attempts to invoke in other contexts result in
     * exceptions or errors possibly including ClassCastException.
     *
     * @return the next task, or null if none are available
     */
    protected static ForkJoinTask<?> peekNextLocalTask() {
        return ((ForkJoinWorkerThread)(Thread.currentThread())).peekTask();
    }

    /**
     * Unschedules and returns, without executing, the next task
     * queued by the current thread but not yet executed.  This method
     * is designed primarily to support extensions, and is unlikely to
     * be useful otherwise.  This method may be invoked only from
     * within ForkJoinTask computations. Attempts to invoke in other
     * contexts result in exceptions or errors possibly including
     * ClassCastException.
     *
     * @return the next task, or null if none are available
     */
    protected static ForkJoinTask<?> pollNextLocalTask() {
        return ((ForkJoinWorkerThread)(Thread.currentThread())).pollLocalTask();
    }

    /**
     * Unschedules and returns, without executing, the next task
     * queued by the current thread but not yet executed, if one is
     * available, or if not available, a task that was forked by some
     * other thread, if available. Availability may be transient, so a
     * <code>null</code> result does not necessarily imply quiecence
     * of the pool this task is operating in.  This method is designed
     * primarily to support extensions, and is unlikely to be useful
     * otherwise.  This method may be invoked only from within
     * ForkJoinTask computations. Attempts to invoke in other contexts
     * result in exceptions or errors possibly including
     * ClassCastException.
     *
     * @return a task, or null if none are available
     */
    protected static ForkJoinTask<?> pollTask() {
        return ((ForkJoinWorkerThread)(Thread.currentThread())).
            pollTask();
    }

    // Serialization support

    private static final long serialVersionUID = -7721805057305804111L;

    /**
     * Save the state to a stream.
     *
     * @serialData the current run status and the exception thrown
     * during execution, or null if none.
     * @param s the stream
     */
    private void writeObject(java.io.ObjectOutputStream s)
        throws java.io.IOException {
        s.defaultWriteObject();
        s.writeObject(getException());
    }

    /**
     * Reconstitute the instance from a stream.
     * @param s the stream
     */
    private void readObject(java.io.ObjectInputStream s)
        throws java.io.IOException, ClassNotFoundException {
        s.defaultReadObject();
        status &= ~INTERNAL_SIGNAL_MASK; // clear internal signal counts
        status |= EXTERNAL_SIGNAL; // conservatively set external signal
        Object ex = s.readObject();
        if (ex != null)
            setDoneExceptionally((Throwable)ex);
    }

    // Temporary Unsafe mechanics for preliminary release
    private static Unsafe getUnsafe() throws Throwable {
        try {
            return Unsafe.getUnsafe();
        } catch (SecurityException se) {
            try {
                return java.security.AccessController.doPrivileged
                    (new java.security.PrivilegedExceptionAction<Unsafe>() {
                        public Unsafe run() throws Exception {
                            return getUnsafePrivileged();
                        }});
            } catch (java.security.PrivilegedActionException e) {
                throw e.getCause();
            }
        }
    }

    private static Unsafe getUnsafePrivileged()
            throws NoSuchFieldException, IllegalAccessException {
        Field f = Unsafe.class.getDeclaredField("theUnsafe");
        f.setAccessible(true);
        return (Unsafe) f.get(null);
    }

    private static long fieldOffset(String fieldName)
            throws NoSuchFieldException {
        return _unsafe.objectFieldOffset
            (ForkJoinTask.class.getDeclaredField(fieldName));
    }

    static final Unsafe _unsafe;
    static final long statusOffset;

    static {
        try {
            _unsafe = getUnsafe();
            statusOffset = fieldOffset("status");
        } catch (Throwable e) {
            throw new RuntimeException("Could not initialize intrinsics", e);
        }
    }

}
