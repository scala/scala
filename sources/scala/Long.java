/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $OldId: Long.java,v 1.14 2002/09/19 10:36:39 paltherr Exp $
// $Id$

package scala;

public abstract class Long extends Float {
    /** @meta method []scala.Long;
     */
    public long $plus() { return asLong(); }
    /** @meta method []scala.Long;
     */
    public long $minus() { return -asLong(); }
    /** @meta method []scala.Long;
     */
    public long $tilde() { return ~asLong(); }

    public long $plus(long that) { return asLong() + that; }
    public long $minus(long that) { return asLong() - that; }
    public long $times(long that) { return asLong() * that; }
    public long $div(long that) { return asLong() / that; }
    public long $percent(long that) { return asLong() % that; }

    public long $less$less(long that) { return asLong() << that; }
    public long $greater$greater(long that) { return asLong() >> that; }
    public long $greater$greater$greater(long that) { return asLong() >>> that; }
    public long $amp(long that) { return asLong() & that; }
    public long $bar(long that) { return asLong() | that; }
    public long $up(long that) { return asLong() ^ that; }

    public boolean $eq$eq(long that) { return asLong() == that; }
    public boolean $bang$eq(long that) { return asLong() != that; }
    public boolean $less(long that) { return asLong() < that; }
    public boolean $greater(long that) { return asLong() > that; }
    public boolean $less$eq(long that) { return asLong() <= that; }
    public boolean $greater$eq(long that) { return asLong() >= that; }

    public float $plus(float that) { return asFloat() + that; }
    public float $minus(float that) { return asFloat() - that; }
    public float $times(float that) { return asFloat() * that; }
    public float $div(float that) { return asFloat() / that; }
    public float $percent(float that) { return asFloat() % that; }

    public boolean $eq$eq(float that) { return asFloat() == that; }
    public boolean $bang$eq(float that) { return asFloat() != that; }
    public boolean $less(float that) { return asFloat() < that; }
    public boolean $greater(float that) { return asFloat() > that; }
    public boolean $less$eq(float that) { return asFloat() <= that; }
    public boolean $greater$eq(float that) { return asFloat() >= that; }

    public double $plus(double that) { return asDouble() + that; }
    public double $minus(double that) { return asDouble() - that; }
    public double $times(double that) { return asDouble() * that; }
    public double $div(double that) { return asDouble() / that; }
    public double $percent(double that) { return asDouble() % that; }

    public boolean $eq$eq(double that) { return asDouble() == that; }
    public boolean $bang$eq(double that) { return asDouble() != that; }
    public boolean $less(double that) { return asDouble() < that; }
    public boolean $greater(double that) { return asDouble() > that; }
    public boolean $less$eq(double that) { return asDouble() <= that; }
    public boolean $greater$eq(double that) { return asDouble() >= that; }

    public String toString() { return String.valueOf(asLong()); }
    public String $plus(String that) { return this + that; }
    public int hashCode() { return (int)(asLong() ^ (asLong() >>> 32)); }
    public boolean equals(java.lang.Object obj) {
        return obj instanceof Long && $eq$eq(((Long)obj).asLong());
    }
}
