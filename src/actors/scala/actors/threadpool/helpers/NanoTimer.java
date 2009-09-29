/*
 * Written by Dawid Kurzyniec and released to the public domain, as explained
 * at http://creativecommons.org/licenses/publicdomain
 */
package scala.actors.threadpool.helpers;

/**
 * Interface to specify custom implementation of precise timer.
 *
 * @author Dawid Kurzyniec
 * @version 1.0
 */
public interface NanoTimer {
    /**
     * Returns the current value of the most precise available system timer,
     * in nanoseconds. This method can only be used to measure elapsed time and
     * is not related to any other notion of system or wall-clock time. The
     * value returned represents nanoseconds since some fixed but arbitrary
     * time (perhaps in the future, so values may be negative). This method
     * provides nanosecond precision, but not necessarily nanosecond accuracy.
     * No guarantees are made about how frequently values change. Differences
     * in successive calls that span greater than approximately 292 years
     * (263 nanoseconds) will not accurately compute elapsed time due to
     * numerical overflow.
     *
     * @return The current value of the system timer, in nanoseconds.
     */
    long nanoTime();
}
