/*
 * Written by Dawid Kurzyniec, based on code written by Doug Lea with assistance
 * from members of JCP JSR-166 Expert Group. Released to the public domain,
 * as explained at http://creativecommons.org/licenses/publicdomain.
 *
 * Thanks to Craig Mattocks for suggesting to use <code>sun.misc.Perf</code>.
 */

package scala.actors.threadpool.helpers;

//import edu.emory.mathcs.backport.java.util.*;
import scala.actors.threadpool.*;
import scala.actors.threadpool.locks.*;
import java.security.AccessController;
import java.security.PrivilegedAction;
import java.lang.reflect.Array;
import java.util.Iterator;
import java.util.Collection;

/**
 * <p>
 * This class groups together the functionality of java.util.concurrent that
 * cannot be fully and reliably implemented in backport, but for which some
 * form of emulation is possible.
 * <p>
 * Currently, this class contains methods related to nanosecond-precision
 * timing, particularly via the {@link #nanoTime} method. To measure time
 * accurately, this method by default uses <code>java.sun.Perf</code> on
 * JDK1.4.2 and it falls back to <code>System.currentTimeMillis</code>
 * on earlier JDKs.
 *
 * @author Dawid Kurzyniec
 * @version 1.0
 */
public final class Utils {

    private final static NanoTimer nanoTimer;
    private final static String providerProp =
        "edu.emory.mathcs.backport.java.util.concurrent.NanoTimerProvider";

    static {
        NanoTimer timer = null;
        try {
            String nanoTimerClassName =
                AccessController.doPrivileged(new PrivilegedAction<String>() {
                    public String run() {
                        return System.getProperty(providerProp);
                    }
                });
            if (nanoTimerClassName != null) {
                Class cls = Class.forName(nanoTimerClassName);
                timer = (NanoTimer) cls.newInstance();
            }
        }
        catch (Exception e) {
            System.err.println("WARNING: unable to load the system-property-defined " +
                               "nanotime provider; switching to the default");
            e.printStackTrace();
        }

        if (timer == null) {
            try {
                timer = new SunPerfProvider();
            }
            catch (Throwable e) {}
        }

        if (timer == null) {
            timer = new MillisProvider();
        }

        nanoTimer = timer;
    }

    private Utils() {}

    /**
     * Returns the current value of the most precise available system timer,
     * in nanoseconds. This method can only be used to measure elapsed time and
     * is not related to any other notion of system or wall-clock time. The
     * value returned represents nanoseconds since some fixed but arbitrary
     * time (perhaps in the future, so values may be negative). This method
     * provides nanosecond precision, but not necessarily nanosecond accuracy.
     * No guarantees are made about how frequently values change. Differences
     * in successive calls that span greater than approximately 292 years
     * (2^63 nanoseconds) will not accurately compute elapsed time due to
     * numerical overflow.
     * <p>
     * <em>Implementation note:</em>By default, this method uses
     * <code>sun.misc.Perf</code> on Java 1.4.2, and falls back to
     * System.currentTimeMillis() emulation on earlier JDKs. Custom
     * timer can be provided via the system property
     * <code>edu.emory.mathcs.backport.java.util.concurrent.NanoTimerProvider</code>.
     * The value of the property should name a class implementing
     * {@link NanoTimer} interface.
     * <p>
     * Note: on JDK 1.4.2, <code>sun.misc.Perf</code> timer seems to have
     * resolution of the order of 1 microsecond, measured on Linux.
     *
     * @return The current value of the system timer, in nanoseconds.
     */
    public static long nanoTime() {
        return nanoTimer.nanoTime();
    }

    /**
     * Causes the current thread to wait until it is signalled or interrupted,
     * or the specified waiting time elapses. This method originally appears
     * in the {@link Condition} interface, but it was moved to here since it
     * can only be emulated, with very little accuracy guarantees: the
     * efficient implementation requires accurate nanosecond timer and native
     * support for nanosecond-precision wait queues, which are not usually
     * present in JVMs prior to 1.5. Loss of precision may cause total waiting
     * times to be systematically shorter than specified when re-waits occur.
     *
     * <p>The lock associated with this condition is atomically
     * released and the current thread becomes disabled for thread scheduling
     * purposes and lies dormant until <em>one</em> of five things happens:
     * <ul>
     * <li>Some other thread invokes the {@link
     * edu.emory.mathcs.backport.java.util.concurrent.locks.Condition#signal}
     * method for this
     * <tt>Condition</tt> and the current thread happens to be chosen as the
     * thread to be awakened; or
     * <li>Some other thread invokes the {@link
     * edu.emory.mathcs.backport.java.util.concurrent.locks.Condition#signalAll}
     * method for this
     * <tt>Condition</tt>; or
     * <li>Some other thread {@link Thread#interrupt interrupts} the current
     * thread, and interruption of thread suspension is supported; or
     * <li>The specified waiting time elapses; or
     * <li>A &quot;<em>spurious wakeup</em>&quot; occurs.
     * </ul>
     *
     * <p>In all cases, before this method can return the current thread must
     * re-acquire the lock associated with this condition. When the
     * thread returns it is <em>guaranteed</em> to hold this lock.
     *
     * <p>If the current thread:
     * <ul>
     * <li>has its interrupted status set on entry to this method; or
     * <li>is {@link Thread#interrupt interrupted} while waiting
     * and interruption of thread suspension is supported,
     * </ul>
     * then {@link InterruptedException} is thrown and the current thread's
     * interrupted status is cleared. It is not specified, in the first
     * case, whether or not the test for interruption occurs before the lock
     * is released.
     *
     * <p>The method returns an estimate of the number of nanoseconds
     * remaining to wait given the supplied <tt>nanosTimeout</tt>
     * value upon return, or a value less than or equal to zero if it
     * timed out. Accuracy of this estimate is directly dependent on the
     * accuracy of {@link #nanoTime}. This value can be used to determine
     * whether and how long to re-wait in cases where the wait returns but an
     * awaited condition still does not hold. Typical uses of this method take
     * the following form:
     *
     * <pre>
     * synchronized boolean aMethod(long timeout, TimeUnit unit) {
     *   long nanosTimeout = unit.toNanos(timeout);
     *   while (!conditionBeingWaitedFor) {
     *     if (nanosTimeout &gt; 0)
     *         nanosTimeout = theCondition.awaitNanos(nanosTimeout);
     *      else
     *        return false;
     *   }
     *   // ...
     * }
     * </pre>
     *
     * <p><b>Implementation Considerations</b>
     * <p>The current thread is assumed to hold the lock associated with this
     * <tt>Condition</tt> when this method is called.
     * It is up to the implementation to determine if this is
     * the case and if not, how to respond. Typically, an exception will be
     * thrown (such as {@link IllegalMonitorStateException}) and the
     * implementation must document that fact.
     *
     * <p>A condition implementation can favor responding to an interrupt over
     * normal method return in response to a signal, or over indicating the
     * elapse of the specified waiting time. In either case the implementation
     * must ensure that the signal is redirected to another waiting thread, if
     * there is one.
     *
     * @param cond the condition to wait for
     * @param nanosTimeout the maximum time to wait, in nanoseconds
     * @return A value less than or equal to zero if the wait has
     * timed out; otherwise an estimate, that
     * is strictly less than the <tt>nanosTimeout</tt> argument,
     * of the time still remaining when this method returned.
     *
     * @throws InterruptedException if the current thread is interrupted (and
     * interruption of thread suspension is supported).
     */
    public static long awaitNanos(Condition cond, long nanosTimeout)
        throws InterruptedException
    {
        if (nanosTimeout <= 0) return nanosTimeout;
        long now = nanoTime();
        cond.await(nanosTimeout, TimeUnit.NANOSECONDS);
        return nanosTimeout - (nanoTime() - now);
    }

    private static final class SunPerfProvider implements NanoTimer {
        final Perf perf;
        final long multiplier, divisor;
        SunPerfProvider() {
            perf =
                AccessController.doPrivileged(new PrivilegedAction<Perf>() {
                    public Perf run() {
                        return Perf.getPerf();
                    }
                });
            // trying to avoid BOTH overflow and rounding errors
            long numerator = 1000000000;
            long denominator = perf.highResFrequency();
            long gcd = gcd(numerator, denominator);
            this.multiplier = numerator / gcd;
            this.divisor = denominator / gcd;
        }
        public long nanoTime() {
            long ctr = perf.highResCounter();

            // anything less sophisticated suffers either from rounding errors
            // (FP arithmetics, backport v1.0) or overflow, when gcd is small
            // (a bug in backport v1.0_01 reported by Ramesh Nethi)

            return ((ctr / divisor) * multiplier) +
                    (ctr % divisor) * multiplier / divisor;

            // even the above can theoretically cause problems if your JVM is
            // running for sufficiently long time, but "sufficiently" means 292
            // years (worst case), or 30,000 years (common case).

            // Details: when the ticks ctr overflows, there is no way to avoid
            // discontinuity in computed nanos, even in infinite arithmetics,
            // unless we count number of overflows that the ctr went through
            // since the JVM started. This follows from the fact that
            // (2^64*multiplier/divisor) mod (2^64) > 0 in general case.
            // Theoretically we could find out the number of overflows by
            // checking System.currentTimeMillis(), but this is unreliable
            // since the system time can unpredictably change during the JVM
            // lifetime.
            // The time to overflow is 2^63 / ticks frequency. With current
            // ticks frequencies of several MHz, it gives about 30,000 years
            // before the problem happens. If ticks frequency reaches 1 GHz, the
            // time to overflow is 292 years. It is unlikely that the frequency
            // ever exceeds 1 GHz. We could double the time to overflow
            // (to 2^64 / frequency) by using unsigned arithmetics, e.g. by
            // adding the following correction whenever the ticks is negative:
            //      -2*((Long.MIN_VALUE / divisor) * multiplier +
            //          (Long.MIN_VALUE % divisor) * multiplier / divisor)
            // But, with the worst case of as much as 292 years, it does not
            // seem justified.
        }
    }

    private static final class MillisProvider implements NanoTimer {
        MillisProvider() {}
        public long nanoTime() {
            return System.currentTimeMillis() * 1000000;
        }
    }

    private static long gcd(long a, long b) {
        long r;
        while (b>0) { r = a % b; a = b; b = r; }
        return a;
    }


    public static Object[] collectionToArray(Collection c) {
        // guess the array size; expect to possibly be different
        int len = c.size();
        Object[] arr = new Object[len];
        Iterator itr = c.iterator();
        int idx = 0;
        while (true) {
            while (idx < len && itr.hasNext()) {
                arr[idx++] = itr.next();
            }
            if (!itr.hasNext()) {
                if (idx == len) return arr;
                // otherwise have to trim
                return Arrays.copyOf(arr, idx, Object[].class);
            }
            // otherwise, have to grow
            int newcap = ((arr.length/2)+1)*3;
            if (newcap < arr.length) {
                // overflow
                if (arr.length < Integer.MAX_VALUE) {
                    newcap = Integer.MAX_VALUE;
                }
                else {
                    throw new OutOfMemoryError("required array size too large");
                }
            }
            arr = Arrays.copyOf(arr, newcap, Object[].class);
            len = newcap;
        }
    }

    public static Object[] collectionToArray(Collection c, Object[] a) {
        Class aType = a.getClass();
        // guess the array size; expect to possibly be different
        int len = c.size();
        Object[] arr = (a.length >= len ? a :
                        (Object[])Array.newInstance(aType.getComponentType(), len));
        Iterator itr = c.iterator();
        int idx = 0;
        while (true) {
            while (idx < len && itr.hasNext()) {
                arr[idx++] = itr.next();
            }
            if (!itr.hasNext()) {
                if (idx == len) return arr;
                if (arr == a) {
                    // orig array -> null terminate
                    a[idx] = null;
                    return a;
                }
                else {
                    // have to trim
                    return Arrays.copyOf(arr, idx, aType);
                }
            }
            // otherwise, have to grow
            int newcap = ((arr.length/2)+1)*3;
            if (newcap < arr.length) {
                // overflow
                if (arr.length < Integer.MAX_VALUE) {
                    newcap = Integer.MAX_VALUE;
                }
                else {
                    throw new OutOfMemoryError("required array size too large");
                }
            }
            arr = Arrays.copyOf(arr, newcap, aType);
            len = newcap;
        }
    }
}
