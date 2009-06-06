/*
 * Written by Doug Lea with assistance from members of JCP JSR-166
 * Expert Group and released to the public domain, as explained at
 * http://creativecommons.org/licenses/publicdomain
 */

package scala.actors.forkjoin;

/**
 * Recursive resultless ForkJoinTasks. This class establishes
 * conventions to parameterize resultless actions as <tt>Void</tt>
 * ForkJoinTasks. Because <tt>null</tt> is the only valid value of
 * <tt>Void</tt>, methods such as join always return <tt>null</tt>
 * upon completion.
 *
 * <p><b>Sample Usages.</b> Here is a sketch of a ForkJoin sort that
 * sorts a given <tt>long[]</tt> array:
 *
 * <pre>
 * class SortTask extends RecursiveAction {
 *   final long[] array; final int lo; final int hi;
 *   SortTask(long[] array, int lo, int hi) {
 *     this.array = array; this.lo = lo; this.hi = hi;
 *   }
 *   protected void compute() {
 *     if (hi - lo &lt; THRESHOLD)
 *       sequentiallySort(array, lo, hi);
 *     else {
 *       int mid = (lo + hi) &gt;&gt;&gt; 1;
 *       invokeAll(new SortTask(array, lo, mid),
 *                 new SortTask(array, mid, hi));
 *       merge(array, lo, hi);
 *     }
 *   }
 * }
 * </pre>
 *
 * You could then sort anArray by creating <tt>new SortTask(anArray, 0,
 * anArray.length-1) </tt> and invoking it in a ForkJoinPool.
 * As a more concrete simple example, the following task increments
 * each element of an array:
 * <pre>
 * class IncrementTask extends RecursiveAction {
 *   final long[] array; final int lo; final int hi;
 *   IncrementTask(long[] array, int lo, int hi) {
 *     this.array = array; this.lo = lo; this.hi = hi;
 *   }
 *   protected void compute() {
 *     if (hi - lo &lt; THRESHOLD) {
 *       for (int i = lo; i &lt; hi; ++i)
 *         array[i]++;
 *     }
 *     else {
 *       int mid = (lo + hi) &gt;&gt;&gt; 1;
 *       invokeAll(new IncrementTask(array, lo, mid),
 *                 new IncrementTask(array, mid, hi));
 *     }
 *   }
 * }
 * </pre>
 *
 *
 * <p>The following example illustrates some refinements and idioms
 * that may lead to better performance: RecursiveActions need not be
 * fully recursive, so long as they maintain the basic
 * divide-and-conquer approach. Here is a class that sums the squares
 * of each element of a double array, by subdividing out only the
 * right-hand-sides of repeated divisions by two, and keeping track of
 * them with a chain of <tt>next</tt> references. It uses a dynamic
 * threshold based on method <tt>surplus</tt>, but counterbalances
 * potential excess partitioning by directly performing leaf actions
 * on unstolen tasks rather than further subdividing.
 *
 * <pre>
 * double sumOfSquares(ForkJoinPool pool, double[] array) {
 *   int n = array.length;
 *   int seqSize = 1 + n / (8 * pool.getParallelism());
 *   Applyer a = new Applyer(array, 0, n, seqSize, null);
 *   pool.invoke(a);
 *   return a.result;
 * }
 *
 * class Applyer extends RecursiveAction {
 *   final double[] array;
 *   final int lo, hi, seqSize;
 *   double result;
 *   Applyer next; // keeps track of right-hand-side tasks
 *   Applyer(double[] array, int lo, int hi, int seqSize, Applyer next) {
 *     this.array = array; this.lo = lo; this.hi = hi;
 *     this.seqSize = seqSize; this.next = next;
 *   }
 *
 *   double atLeaf(int l, int r) {
 *     double sum = 0;
 *     for (int i = l; i &lt; h; ++i) // perform leftmost base step
 *       sum += array[i] * array[i];
 *     return sum;
 *   }
 *
 *   protected void compute() {
 *     int l = lo;
 *     int h = hi;
 *     Applyer right = null;
 *     while (h - l &gt; 1 &amp;&amp;
 *        ForkJoinWorkerThread.getEstimatedSurplusTaskCount() &lt;= 3) {
 *        int mid = (l + h) &gt;&gt;&gt; 1;
 *        right = new Applyer(array, mid, h, seqSize, right);
 *        right.fork();
 *        h = mid;
 *     }
 *     double sum = atLeaf(l, h);
 *     while (right != null) {
 *        if (right.tryUnfork()) // directly calculate if not stolen
 *          sum += right.atLeaf(right.lo, right.hi);
 *       else {
 *          right.helpJoin();
 *          sum += right.result;
 *        }
 *        right = right.next;
 *      }
 *     result = sum;
 *   }
 * }
 * </pre>
 */
public abstract class RecursiveAction extends ForkJoinTask<Void> {

    /**
     * The main computation performed by this task.
     */
    protected abstract void compute();

    /**
     * Always returns null
     */
    public final Void getRawResult() { return null; }

    /**
     * Requires null completion value.
     */
    protected final void setRawResult(Void mustBeNull) { }

    /**
     * Implements execution conventions for RecursiveActions
     */
    protected final boolean exec() {
        compute();
        return true;
    }

}
