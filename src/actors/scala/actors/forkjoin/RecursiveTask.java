/*
 * Written by Doug Lea with assistance from members of JCP JSR-166
 * Expert Group and released to the public domain, as explained at
 * http://creativecommons.org/licenses/publicdomain
 */

package scala.actors.forkjoin;

/**
 * Recursive result-bearing ForkJoinTasks.
 * <p> For a classic example, here is a task computing Fibonacci numbers:
 *
 * <pre>
 * class Fibonacci extends RecursiveTask&lt;Integer&gt; {
 *   final int n;
 *   Fibonnaci(int n) { this.n = n; }
 *   Integer compute() {
 *     if (n &lt;= 1)
 *        return n;
 *     Fibonacci f1 = new Fibonacci(n - 1);
 *     f1.fork();
 *     Fibonacci f2 = new Fibonacci(n - 2);
 *     return f2.compute() + f1.join();
 *   }
 * }
 * </pre>
 *
 * However, besides being a dumb way to compute Fibonacci functions
 * (there is a simple fast linear algorithm that you'd use in
 * practice), this is likely to perform poorly because the smallest
 * subtasks are too small to be worthwhile splitting up. Instead, as
 * is the case for nearly all fork/join applications, you'd pick some
 * minimum granularity size (for example 10 here) for which you always
 * sequentially solve rather than subdividing.
 *
 */
public abstract class RecursiveTask<V> extends ForkJoinTask<V> {

    /**
     * Empty contructor for use by subclasses.
     */
    protected RecursiveTask() {
    }

    /**
     * The result returned by compute method.
     */
    V result;

    /**
     * The main computation performed by this task.
     */
    protected abstract V compute();

    public final V getRawResult() {
        return result;
    }

    protected final void setRawResult(V value) {
        result = value;
    }

    /**
     * Implements execution conventions for RecursiveTask
     */
    protected final boolean exec() {
        result = compute();
        return true;
    }

}
