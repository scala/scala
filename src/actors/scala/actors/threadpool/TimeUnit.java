/*
 * Written by Doug Lea with assistance from members of JCP JSR-166
 * Expert Group and released to the public domain, as explained at
 * http://creativecommons.org/licenses/publicdomain
 */

package scala.actors.threadpool;

import java.io.InvalidObjectException;
import java.io.ObjectStreamException;

/**
  * A <tt>TimeUnit</tt> represents time durations at a given unit of
  * granularity and provides utility methods to convert across units,
  * and to perform timing and delay operations in these units.  A
  * <tt>TimeUnit</tt> does not maintain time information, but only
  * helps organize and use time representations that may be maintained
  * separately across various contexts.  A nanosecond is defined as one
  * thousandth of a microsecond, a microsecond as one thousandth of a
  * millisecond, a millisecond as one thousandth of a second, a minute
  * as sixty seconds, an hour as sixty minutes, and a day as twenty four
  * hours.
  *
  * <p>A <tt>TimeUnit</tt> is mainly used to inform time-based methods
  * how a given timing parameter should be interpreted. For example,
  * the following code will timeout in 50 milliseconds if the {@link
  * edu.emory.mathcs.backport.java.util.concurrent.locks.Lock lock} is not available:
  *
  * <pre>  Lock lock = ...;
  *  if ( lock.tryLock(50L, TimeUnit.MILLISECONDS) ) ...
  * </pre>
  * while this code will timeout in 50 seconds:
  * <pre>
  *  Lock lock = ...;
  *  if ( lock.tryLock(50L, TimeUnit.SECONDS) ) ...
  * </pre>
  *
  * Note however, that there is no guarantee that a particular timeout
  * implementation will be able to notice the passage of time at the
  * same granularity as the given <tt>TimeUnit</tt>.
  *
  * @since 1.5
  * @author Doug Lea
  */
public abstract class TimeUnit implements java.io.Serializable {

    public static final TimeUnit NANOSECONDS = new TimeUnit(0, "NANOSECONDS") {
        private final static long serialVersionUID = 535148490883208361L;
        public long toNanos(long d)   { return d; }
        public long toMicros(long d)  { return d/(C1/C0); }
        public long toMillis(long d)  { return d/(C2/C0); }
        public long toSeconds(long d) { return d/(C3/C0); }
        public long toMinutes(long d) { return d/(C4/C0); }
        public long toHours(long d)   { return d/(C5/C0); }
        public long toDays(long d)    { return d/(C6/C0); }
        public long convert(long d, TimeUnit u) { return u.toNanos(d); }
        int excessNanos(long d, long m) { return (int)(d - (m*C2)); }
    };
    public static final TimeUnit MICROSECONDS = new TimeUnit(1, "MICROSECONDS") {
        private final static long serialVersionUID = 2185906575929579108L;
        public long toNanos(long d)   { return x(d, C1/C0, MAX/(C1/C0)); }
        public long toMicros(long d)  { return d; }
        public long toMillis(long d)  { return d/(C2/C1); }
        public long toSeconds(long d) { return d/(C3/C1); }
        public long toMinutes(long d) { return d/(C4/C1); }
        public long toHours(long d)   { return d/(C5/C1); }
        public long toDays(long d)    { return d/(C6/C1); }
        public long convert(long d, TimeUnit u) { return u.toMicros(d); }
        int excessNanos(long d, long m) { return (int)((d*C1) - (m*C2)); }
    };
    public static final TimeUnit MILLISECONDS = new TimeUnit(2, "MILLISECONDS") {
        private final static long serialVersionUID = 9032047794123325184L;
        public long toNanos(long d)   { return x(d, C2/C0, MAX/(C2/C0)); }
        public long toMicros(long d)  { return x(d, C2/C1, MAX/(C2/C1)); }
        public long toMillis(long d)  { return d; }
        public long toSeconds(long d) { return d/(C3/C2); }
        public long toMinutes(long d) { return d/(C4/C2); }
        public long toHours(long d)   { return d/(C5/C2); }
        public long toDays(long d)    { return d/(C6/C2); }
        public long convert(long d, TimeUnit u) { return u.toMillis(d); }
        int excessNanos(long d, long m) { return 0; }
    };
    public static final TimeUnit SECONDS = new TimeUnit(3, "SECONDS") {
        private final static long serialVersionUID = 227755028449378390L;
        public long toNanos(long d)   { return x(d, C3/C0, MAX/(C3/C0)); }
        public long toMicros(long d)  { return x(d, C3/C1, MAX/(C3/C1)); }
        public long toMillis(long d)  { return x(d, C3/C2, MAX/(C3/C2)); }
        public long toSeconds(long d) { return d; }
        public long toMinutes(long d) { return d/(C4/C3); }
        public long toHours(long d)   { return d/(C5/C3); }
        public long toDays(long d)    { return d/(C6/C3); }
        public long convert(long d, TimeUnit u) { return u.toSeconds(d); }
        int excessNanos(long d, long m) { return 0; }
    };
    public static final TimeUnit MINUTES = new TimeUnit(4, "MINUTES") {
        private final static long serialVersionUID = 1827351566402609187L;
        public long toNanos(long d)   { return x(d, C4/C0, MAX/(C4/C0)); }
        public long toMicros(long d)  { return x(d, C4/C1, MAX/(C4/C1)); }
        public long toMillis(long d)  { return x(d, C4/C2, MAX/(C4/C2)); }
        public long toSeconds(long d) { return x(d, C4/C3, MAX/(C4/C3)); }
        public long toMinutes(long d) { return d; }
        public long toHours(long d)   { return d/(C5/C4); }
        public long toDays(long d)    { return d/(C6/C4); }
        public long convert(long d, TimeUnit u) { return u.toMinutes(d); }
        int excessNanos(long d, long m) { return 0; }
    };
    public static final TimeUnit HOURS = new TimeUnit(5, "HOURS") {
        private final static long serialVersionUID = -6438436134732089810L;
        public long toNanos(long d)   { return x(d, C5/C0, MAX/(C5/C0)); }
        public long toMicros(long d)  { return x(d, C5/C1, MAX/(C5/C1)); }
        public long toMillis(long d)  { return x(d, C5/C2, MAX/(C5/C2)); }
        public long toSeconds(long d) { return x(d, C5/C3, MAX/(C5/C3)); }
        public long toMinutes(long d) { return x(d, C5/C4, MAX/(C5/C4)); }
        public long toHours(long d)   { return d; }
        public long toDays(long d)    { return d/(C6/C5); }
        public long convert(long d, TimeUnit u) { return u.toHours(d); }
        int excessNanos(long d, long m) { return 0; }
    };
    public static final TimeUnit DAYS = new TimeUnit(6, "DAYS") {
        private final static long serialVersionUID = 567463171959674600L;
        public long toNanos(long d)   { return x(d, C6/C0, MAX/(C6/C0)); }
        public long toMicros(long d)  { return x(d, C6/C1, MAX/(C6/C1)); }
        public long toMillis(long d)  { return x(d, C6/C2, MAX/(C6/C2)); }
        public long toSeconds(long d) { return x(d, C6/C3, MAX/(C6/C3)); }
        public long toMinutes(long d) { return x(d, C6/C4, MAX/(C6/C4)); }
        public long toHours(long d)   { return x(d, C6/C5, MAX/(C6/C5)); }
        public long toDays(long d)    { return d; }
        public long convert(long d, TimeUnit u) { return u.toDays(d); }
        int excessNanos(long d, long m) { return 0; }
    };

    private static final TimeUnit[] values = new TimeUnit[]
        { NANOSECONDS, MICROSECONDS, MILLISECONDS, SECONDS, MINUTES, HOURS, DAYS };

    public static TimeUnit[] values() {
        return (TimeUnit[])values.clone();
    }

    /**
     * Returns the enum constant of this type with the specified name. The
     * string must match <em>exactly</em> an identifier used to declare an
     * enum constant in this type. (Extraneous whitespace characters are not
     * permitted.)
     *
     * @param name the name of the enum constant to be returned
     * @return the enum constant with the specified name
     * @throws IllegalArgumentException
     *         if this enum type has no constant with the specified name
     */
    public static TimeUnit valueOf(String name) {
        for (int i = 0; i < values.length; i++) {
            if (values[i].name.equals(name)) {
                return values[i];
            }
        }
        throw new IllegalArgumentException("No enum const TimeUnit." + name);
    }

    /**
     * The ordinal of this unit. This is useful both for {@link #ordinal()}
     * and to maintain serialization consistence with earlier versions.
     */
    private final int index;

    /** name of this unit */
    private final String name;

    /** Internal constructor */
    TimeUnit(int index, String name) {
        this.index = index;
        this.name = name;
    }

    // Handy constants for conversion methods
    static final long C0 = 1;
    static final long C1 = C0 * 1000;
    static final long C2 = C1 * 1000;
    static final long C3 = C2 * 1000;
    static final long C4 = C3 * 60;
    static final long C5 = C4 * 60;
    static final long C6 = C5 * 24;

    static final long MAX = Long.MAX_VALUE;

    /**
     * Scale d by m, checking for overflow.
     * This has a short name to make above code more readable.
     */
    static long x(long d, long m, long over) {
        if (d >  over) return Long.MAX_VALUE;
        if (d < -over) return Long.MIN_VALUE;
        return d * m;
    }

    /**
     * Convert the given time duration in the given unit to this
     * unit.  Conversions from finer to coarser granularities
     * truncate, so lose precision. For example converting
     * <tt>999</tt> milliseconds to seconds results in
     * <tt>0</tt>. Conversions from coarser to finer granularities
     * with arguments that would numerically overflow saturate to
     * <tt>Long.MIN_VALUE</tt> if negative or <tt>Long.MAX_VALUE</tt>
     * if positive.
     *
     * <p>For example, to convert 10 minutes to milliseconds, use:
     * <tt>TimeUnit.MILLISECONDS.convert(10L, TimeUnit.MINUTES)</tt>
     *
     * @param sourceDuration the time duration in the given <tt>sourceUnit</tt>
     * @param sourceUnit the unit of the <tt>sourceDuration</tt> argument
     * @return the converted duration in this unit,
     * or <tt>Long.MIN_VALUE</tt> if conversion would negatively
     * overflow, or <tt>Long.MAX_VALUE</tt> if it would positively overflow.
     */
    public abstract long convert(long sourceDuration, TimeUnit sourceUnit);

    /**
     * Equivalent to <tt>NANOSECONDS.convert(duration, this)</tt>.
     * @param duration the duration
     * @return the converted duration,
     * or <tt>Long.MIN_VALUE</tt> if conversion would negatively
     * overflow, or <tt>Long.MAX_VALUE</tt> if it would positively overflow.
     * @see #convert
     */
    public abstract long toNanos(long duration);

    /**
     * Equivalent to <tt>MICROSECONDS.convert(duration, this)</tt>.
     * @param duration the duration
     * @return the converted duration,
     * or <tt>Long.MIN_VALUE</tt> if conversion would negatively
     * overflow, or <tt>Long.MAX_VALUE</tt> if it would positively overflow.
     * @see #convert
     */
    public abstract long toMicros(long duration);

    /**
     * Equivalent to <tt>MILLISECONDS.convert(duration, this)</tt>.
     * @param duration the duration
     * @return the converted duration,
     * or <tt>Long.MIN_VALUE</tt> if conversion would negatively
     * overflow, or <tt>Long.MAX_VALUE</tt> if it would positively overflow.
     * @see #convert
     */
    public abstract long toMillis(long duration);

    /**
     * Equivalent to <tt>SECONDS.convert(duration, this)</tt>.
     * @param duration the duration
     * @return the converted duration,
     * or <tt>Long.MIN_VALUE</tt> if conversion would negatively
     * overflow, or <tt>Long.MAX_VALUE</tt> if it would positively overflow.
     * @see #convert
     */
    public abstract long toSeconds(long duration);

    /**
     * Equivalent to <tt>MINUTES.convert(duration, this)</tt>.
     * @param duration the duration
     * @return the converted duration,
     * or <tt>Long.MIN_VALUE</tt> if conversion would negatively
     * overflow, or <tt>Long.MAX_VALUE</tt> if it would positively overflow.
     * @see #convert
     * @since 1.6
     */
    public abstract long toMinutes(long duration);

    /**
     * Equivalent to <tt>HOURS.convert(duration, this)</tt>.
     * @param duration the duration
     * @return the converted duration,
     * or <tt>Long.MIN_VALUE</tt> if conversion would negatively
     * overflow, or <tt>Long.MAX_VALUE</tt> if it would positively overflow.
     * @see #convert
     * @since 1.6
     */
    public abstract long toHours(long duration);

    /**
     * Equivalent to <tt>DAYS.convert(duration, this)</tt>.
     * @param duration the duration
     * @return the converted duration
     * @see #convert
     * @since 1.6
     */
    public abstract long toDays(long duration);

    /**
     * Utility to compute the excess-nanosecond argument to wait,
     * sleep, join.
     * @param d the duration
     * @param m the number of milliseconds
     * @return the number of nanoseconds
     */
    abstract int excessNanos(long d, long m);

    /**
     * Returns the name of this enum constant, exactly as declared in its enum
     * declaration. <strong>Most programmers should use the
     * {@link #toString()} method in preference to this one, as the toString
     * method may return a more user-friendly name.</strong> This method is
     * designed primarily for use in specialized situations where correctness
     * depends on getting the exact name, which will not vary from release to
     * release.
     *
     * @return the name of this enum constant
     */
    public String name() {
        return name;
    }

    /**
     * Returns the ordinal of this enumeration constant (its position in its
     * enum declaration, where the initial constant is assigned an ordinal of
     * zero). Most programmers will have no use for this method. It is
     * designed for use by sophisticated enum-based data structures, such as
     * <code>EnumSet</code> and <code>EnumMap</code>.
     *
     * @return the ordinal of this enumeration constant
     */
    public int ordinal() {
        return index;
    }

    /*
     * Guarantees that deserialized objects will be referentially equal to the
     * standard enumeration objects.
     */
    protected Object readResolve() throws ObjectStreamException {
        try {
            return valueOf(name);
        } catch (IllegalArgumentException e) {
            throw new InvalidObjectException(name
                    + " is not a valid enum for TimeUnit");
        }
    }

    /**
     * Performs a timed <tt>Object.wait</tt> using this time unit.
     * This is a convenience method that converts timeout arguments
     * into the form required by the <tt>Object.wait</tt> method.
     *
     * <p>For example, you could implement a blocking <tt>poll</tt>
     * method (see {@link BlockingQueue#poll BlockingQueue.poll})
     * using:
     *
     * <pre>  public synchronized  Object poll(long timeout, TimeUnit unit) throws InterruptedException {
     *    while (empty) {
     *      unit.timedWait(this, timeout);
     *      ...
     *    }
     *  }</pre>
     *
     * @param obj the object to wait on
     * @param timeout the maximum time to wait. If less than
     * or equal to zero, do not wait at all.
     * @throws InterruptedException if interrupted while waiting.
     * @see java.lang.Object#wait(long, int)
     */
    public void timedWait(Object obj, long timeout)
        throws InterruptedException {
        if (timeout > 0) {
            long ms = toMillis(timeout);
            int ns = excessNanos(timeout, ms);
            obj.wait(ms, ns);
        }
    }

    /**
     * Performs a timed <tt>Thread.join</tt> using this time unit.
     * This is a convenience method that converts time arguments into the
     * form required by the <tt>Thread.join</tt> method.
     * @param thread the thread to wait for
     * @param timeout the maximum time to wait. If less than
     * or equal to zero, do not wait at all.
     * @throws InterruptedException if interrupted while waiting.
     * @see java.lang.Thread#join(long, int)
     */
    public void timedJoin(Thread thread, long timeout)
        throws InterruptedException {
        if (timeout > 0) {
            long ms = toMillis(timeout);
            int ns = excessNanos(timeout, ms);
            thread.join(ms, ns);
        }
    }

    /**
     * Performs a <tt>Thread.sleep</tt> using this unit.
     * This is a convenience method that converts time arguments into the
     * form required by the <tt>Thread.sleep</tt> method.
     * @param timeout the maximum time to sleep. If less than
     * or equal to zero, do not sleep at all.
     * @throws InterruptedException if interrupted while sleeping.
     * @see java.lang.Thread#sleep
     */
    public void sleep(long timeout) throws InterruptedException {
        if (timeout > 0) {
            long ms = toMillis(timeout);
            int ns = excessNanos(timeout, ms);
            Thread.sleep(ms, ns);
        }
    }

    public String toString() {
        return name;
    }
}
