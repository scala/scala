/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $OldId: Float.java,v 1.13 2002/09/19 10:36:39 paltherr Exp $
// $Id$

package scala;


public abstract class Float extends Double {
    /** @meta method []scala.Float;
     */
    public float $plus() { return asFloat(); }
    /** @meta method []scala.Float;
     */
    public float $minus() { return -asFloat(); }

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

    public String toString() { return String.valueOf(asFloat()); }
    public String $plus(String that) { return this + that; }
    public int hashCode() { return java.lang.Float.floatToIntBits(asFloat()); }
    public boolean equals(java.lang.Object obj) {
        return obj instanceof Float && $eq$eq(((Float)obj).asFloat());
    }
}
