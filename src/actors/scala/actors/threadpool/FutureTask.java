/*
 * Written by Doug Lea with assistance from members of JCP JSR-166
 * Expert Group and released to the public domain. Use, modify, and
 * redistribute this code in any way without acknowledgement.
 */

package scala.actors.threadpool;

import scala.actors.threadpool.*; // for javadoc
import scala.actors.threadpool.helpers.*;

/**
 * A cancellable asynchronous computation.  This class provides a base
 * implementation of {@link Future}, with methods to start and cancel
 * a computation, query to see if the computation is complete, and
 * retrieve the result of the computation.  The result can only be
 * retrieved when the computation has completed; the <tt>get</tt>
 * method will block if the computation has not yet completed.  Once
 * the computation has completed, the computation cannot be restarted
 * or cancelled.
 *
 * <p>A <tt>FutureTask</tt> can be used to wrap a {@link Callable} or
 * {@link java.lang.Runnable} object.  Because <tt>FutureTask</tt>
 * implements <tt>Runnable</tt>, a <tt>FutureTask</tt> can be
 * submitted to an {@link Executor} for execution.
 *
 * <p>In addition to serving as a standalone class, this class provides
 * <tt>protected</tt> functionality that may be useful when creating
 * customized task classes.
 *
 * @since 1.5
 * @author Doug Lea
 */
public class FutureTask implements RunnableFuture {

    /** State value representing that task is ready to run */
    private static final int READY     = 0;
    /** State value representing that task is running */
    private static final int RUNNING   = 1;
    /** State value representing that task ran */
    private static final int RAN       = 2;
    /** State value representing that task was cancelled */
    private static final int CANCELLED = 4;

    /** The underlying callable */
    private final Callable callable;
    /** The result to return from get() */
    private Object result;
    /** The exception to throw from get() */
    private Throwable exception;

    private int state;

    /**
     * The thread running task. When nulled after set/cancel, this
     * indicates that the results are accessible.  Must be
     * volatile, to ensure visibility upon completion.
     */
    private volatile Thread runner;

    /**
     * Creates a <tt>FutureTask</tt> that will, upon running, execute the
     * given <tt>Callable</tt>.
     *
     * @param  callable the callable task
     * @throws NullPointerException if callable is null
     */
    public FutureTask(Callable callable) {
        if (callable == null)
            throw new NullPointerException();
        this.callable = callable;
    }

    /**
     * Creates a <tt>FutureTask</tt> that will, upon running, execute the
     * given <tt>Runnable</tt>, and arrange that <tt>get</tt> will return the
     * given result on successful completion.
     *
     * @param  runnable the runnable task
     * @param result the result to return on successful completion. If
     * you don't need a particular result, consider using
     * constructions of the form:
     * <tt>Future&lt;?&gt; f = new FutureTask&lt;Object&gt;(runnable, null)</tt>
     * @throws NullPointerException if runnable is null
     */
    public FutureTask(Runnable runnable, Object result) {
        this(Executors.callable(runnable, result));
    }

    public synchronized boolean isCancelled() {
        return state == CANCELLED;
    }

    public synchronized boolean isDone() {
        return ranOrCancelled() && runner == null;
    }

    public boolean cancel(boolean mayInterruptIfRunning) {
        synchronized (this) {
            if (ranOrCancelled()) return false;
            state = CANCELLED;
            if (mayInterruptIfRunning) {
                Thread r = runner;
                if (r != null) r.interrupt();
            }
            runner = null;
            notifyAll();
        }
        done();
        return true;
    }

    /**
     * @throws CancellationException {@inheritDoc}
     */
    public synchronized Object get()
        throws InterruptedException, ExecutionException
    {
        waitFor();
        return getResult();
    }

    /**
     * @throws CancellationException {@inheritDoc}
     */
    public synchronized Object get(long timeout, TimeUnit unit)
        throws InterruptedException, ExecutionException, TimeoutException
    {
        waitFor(unit.toNanos(timeout));
        return getResult();
    }

    /**
     * Protected method invoked when this task transitions to state
     * <tt>isDone</tt> (whether normally or via cancellation). The
     * default implementation does nothing.  Subclasses may override
     * this method to invoke completion callbacks or perform
     * bookkeeping. Note that you can query status inside the
     * implementation of this method to determine whether this task
     * has been cancelled.
     */
    protected void done() { }

    /**
     * Sets the result of this Future to the given value unless
     * this future has already been set or has been cancelled.
     * This method is invoked internally by the <tt>run</tt> method
     * upon successful completion of the computation.
     * @param v the value
     */
    protected void set(Object v) {
        setCompleted(v);
    }

    /**
     * Causes this future to report an <tt>ExecutionException</tt>
     * with the given throwable as its cause, unless this Future has
     * already been set or has been cancelled.
     * This method is invoked internally by the <tt>run</tt> method
     * upon failure of the computation.
     * @param t the cause of failure
     */
    protected void setException(Throwable t) {
        setFailed(t);
    }

    /**
     * Sets this Future to the result of its computation
     * unless it has been cancelled.
     */
    public void run() {
        synchronized (this) {
            if (state != READY) return;
            state = RUNNING;
            runner = Thread.currentThread();
        }
        try {
            set(callable.call());
        }
        catch (Throwable ex) {
            setException(ex);
        }
    }

    /**
     * Executes the computation without setting its result, and then
     * resets this Future to initial state, failing to do so if the
     * computation encounters an exception or is cancelled.  This is
     * designed for use with tasks that intrinsically execute more
     * than once.
     * @return true if successfully run and reset
     */
    protected boolean runAndReset() {
        synchronized (this) {
            if (state != READY) return false;
            state = RUNNING;
            runner = Thread.currentThread();
        }
        try {
            callable.call(); // don't set result
            synchronized (this) {
                runner = null;
                if (state == RUNNING) {
                    state = READY;
                    return true;
                }
                else {
                    return false;
                }
            }
        }
        catch (Throwable ex) {
            setException(ex);
            return false;
        }
    }

    // PRE: lock owned
    private boolean ranOrCancelled() {
        return (state & (RAN | CANCELLED)) != 0;
    }

    /**
     * Marks the task as completed.
     * @param result the result of a task.
     */
    private void setCompleted(Object result) {
        synchronized (this) {
            if (ranOrCancelled()) return;
            this.state = RAN;
            this.result = result;
            this.runner = null;
            notifyAll();
        }

        // invoking callbacks *after* setting future as completed and
        // outside the synchronization block makes it safe to call
        // interrupt() from within callback code (in which case it will be
        // ignored rather than cause deadlock / illegal state exception)
        done();
    }

    /**
     * Marks the task as failed.
     * @param exception the cause of abrupt completion.
     */
    private void setFailed(Throwable exception) {
        synchronized (this) {
            if (ranOrCancelled()) return;
            this.state = RAN;
            this.exception = exception;
            this.runner = null;
            notifyAll();
        }

        // invoking callbacks *after* setting future as completed and
        // outside the synchronization block makes it safe to call
        // interrupt() from within callback code (in which case it will be
        // ignored rather than cause deadlock / illegal state exception)
        done();
    }

    /**
     * Waits for the task to complete.
     * PRE: lock owned
     */
    private void waitFor() throws InterruptedException {
        while (!isDone()) {
            wait();
        }
    }

    /**
     * Waits for the task to complete for timeout nanoseconds or throw
     * TimeoutException if still not completed after that
     * PRE: lock owned
     */
    private void waitFor(long nanos) throws InterruptedException, TimeoutException {
        if (nanos < 0) throw new IllegalArgumentException();
        if (isDone()) return;
        long deadline = Utils.nanoTime() + nanos;
        while (nanos > 0) {
            TimeUnit.NANOSECONDS.timedWait(this, nanos);
            if (isDone()) return;
            nanos = deadline - Utils.nanoTime();
        }
        throw new TimeoutException();
    }

    /**
     * Gets the result of the task.
     *
     * PRE: task completed
     * PRE: lock owned
     */
    private Object getResult() throws ExecutionException {
        if (state == CANCELLED) {
            throw new CancellationException();
        }
        if (exception != null) {
            throw new ExecutionException(exception);
        }
        return result;
    }

    // todo: consider
    //public String toString() {
    //    return callable.toString();
    //}
}
