/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $OldId: Double.java,v 1.13 2002/10/11 08:17:20 schinz Exp $
// $Id$

package scala;


public abstract class Double extends AnyVal {
    public abstract double asDouble();
    public abstract float asFloat();
    public abstract long asLong();
    public abstract int asInt();
    public abstract char asChar();
    public abstract short asShort();
    public abstract byte asByte();

    // The following definitions are *default* definitions for the
    // "is" methods, and *not* the correct definitions for the
    // "Double" class. The correct definitions for the real "Double"
    // class are to be found in "RunTime.java".
    public boolean isDouble() { return true; }
    public boolean isFloat() { return true; }
    public boolean isLong() { return true; }
    public boolean isInt() { return true; }
    public boolean isChar() { return true; }
    public boolean isShort() { return true; }
    public boolean isByte() { return true; }

    /** @meta method []scala.Double;
     */
    public double $plus() { return asDouble(); }
    /** @meta method []scala.Double;
     */
    public double $minus() { return -asDouble(); }

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

    public String toString() { return String.valueOf(asDouble()); }
    public String $plus(String that) { return this + that; }
    public int hashCode() {
        long bits = java.lang.Double.doubleToLongBits(asDouble());
        return (int)(bits ^ (bits >>> 32));
    }
    public boolean equals(java.lang.Object obj) {
        return obj instanceof Double && $eq$eq(((Double)obj).asDouble());
    }
}
