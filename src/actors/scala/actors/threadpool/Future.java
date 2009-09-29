/*
 * Written by Doug Lea with assistance from members of JCP JSR-166
 * Expert Group and released to the public domain, as explained at
 * http://creativecommons.org/licenses/publicdomain
 */

package scala.actors.threadpool;
import scala.actors.threadpool.*; // for javadoc (till 6280605 is fixed)

/**
 * A <tt>Future</tt> represents the result of an asynchronous
 * computation.  Methods are provided to check if the computation is
 * complete, to wait for its completion, and to retrieve the result of
 * the computation.  The result can only be retrieved using method
 * <tt>get</tt> when the computation has completed, blocking if
 * necessary until it is ready.  Cancellation is performed by the
 * <tt>cancel</tt> method.  Additional methods are provided to
 * determine if the task completed normally or was cancelled. Once a
 * computation has completed, the computation cannot be cancelled.
 * If you would like to use a <tt>Future</tt> for the sake
 * of cancellability but not provide a usable result, you can
 * declare types of the form <tt>Future&lt;?&gt;</tt> and
 * return <tt>null</tt> as a result of the underlying task.
 *
 * <p>
 * <b>Sample Usage</b> (Note that the following classes are all
 * made-up.) <p>
 * <pre>
 * interface ArchiveSearcher { String search(String target); }
 * class App {
 *   ExecutorService executor = ...
 *   ArchiveSearcher searcher = ...
 *   void showSearch(final String target)
 *       throws InterruptedException {
 *     Future&lt;String&gt; future
 *       = executor.submit(new Callable&lt;String&gt;() {
 *         public String call() {
 *             return searcher.search(target);
 *         }});
 *     displayOtherThings(); // do other things while searching
 *     try {
 *       displayText(future.get()); // use future
 *     } catch (ExecutionException ex) { cleanup(); return; }
 *   }
 * }
 * </pre>
 *
 * The {@link FutureTask} class is an implementation of <tt>Future</tt> that
 * implements <tt>Runnable</tt>, and so may be executed by an <tt>Executor</tt>.
 * For example, the above construction with <tt>submit</tt> could be replaced by:
 * <pre>
 *     FutureTask&lt;String&gt; future =
 *       new FutureTask&lt;String&gt;(new Callable&lt;String&gt;() {
 *         public String call() {
 *           return searcher.search(target);
 *       }});
 *     executor.execute(future);
 * </pre>
 *
 * <p>Memory consistency effects: Actions taken by the asynchronous computation
 * <a href="package-summary.html#MemoryVisibility"> <i>happen-before</i></a>
 * actions following the corresponding {@code Future.get()} in another thread.
 *
 * @see FutureTask
 * @see Executor
 * @since 1.5
 * @author Doug Lea
 */
public interface Future {

    /**
     * Attempts to cancel execution of this task.  This attempt will
     * fail if the task has already completed, has already been cancelled,
     * or could not be cancelled for some other reason. If successful,
     * and this task has not started when <tt>cancel</tt> is called,
     * this task should never run.  If the task has already started,
     * then the <tt>mayInterruptIfRunning</tt> parameter determines
     * whether the thread executing this task should be interrupted in
     * an attempt to stop the task.
     *
     * <p>After this method returns, subsequent calls to {@link #isDone} will
     * always return <tt>true</tt>.  Subsequent calls to {@link #isCancelled}
     * will always return <tt>true</tt> if this method returned <tt>true</tt>.
     *
     * @param mayInterruptIfRunning <tt>true</tt> if the thread executing this
     * task should be interrupted; otherwise, in-progress tasks are allowed
     * to complete
     * @return <tt>false</tt> if the task could not be cancelled,
     * typically because it has already completed normally;
     * <tt>true</tt> otherwise
     */
    boolean cancel(boolean mayInterruptIfRunning);

    /**
     * Returns <tt>true</tt> if this task was cancelled before it completed
     * normally.
     *
     * @return <tt>true</tt> if this task was cancelled before it completed
     */
    boolean isCancelled();

    /**
     * Returns <tt>true</tt> if this task completed.
     *
     * Completion may be due to normal termination, an exception, or
     * cancellation -- in all of these cases, this method will return
     * <tt>true</tt>.
     *
     * @return <tt>true</tt> if this task completed
     */
    boolean isDone();

    /**
     * Waits if necessary for the computation to complete, and then
     * retrieves its result.
     *
     * @return the computed result
     * @throws CancellationException if the computation was cancelled
     * @throws ExecutionException if the computation threw an
     * exception
     * @throws InterruptedException if the current thread was interrupted
     * while waiting
     */
    Object get() throws InterruptedException, ExecutionException;

    /**
     * Waits if necessary for at most the given time for the computation
     * to complete, and then retrieves its result, if available.
     *
     * @param timeout the maximum time to wait
     * @param unit the time unit of the timeout argument
     * @return the computed result
     * @throws CancellationException if the computation was cancelled
     * @throws ExecutionException if the computation threw an
     * exception
     * @throws InterruptedException if the current thread was interrupted
     * while waiting
     * @throws TimeoutException if the wait timed out
     */
    Object get(long timeout, TimeUnit unit)
        throws InterruptedException, ExecutionException, TimeoutException;
}
