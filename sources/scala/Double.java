/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala;

public abstract class Double extends AnyVal {












    public abstract double asDouble();
    public abstract float  asFloat();
    public abstract long   asLong();
    public abstract int    asInt();
    public abstract char   asChar();
    public abstract short  asShort();
    public abstract byte   asByte();

    public boolean equals(java.lang.Object other) {
        return other instanceof Double && $eq$eq(((Double)other).asDouble());
    }
    public int hashCode() {
        long bits = java.lang.Double.doubleToLongBits(asDouble());
        return (int)(bits ^ (bits >>> 32));
    }
    public String toString() {
        return String.valueOf(asDouble());
    }

    /** @meta method (scala.Any)scala.Boolean; */
    public boolean $eq$eq  (java.lang.Object other) { return  equals(other); }
    /** @meta method (scala.Any)scala.Boolean; */
    public boolean $bang$eq(java.lang.Object other) { return !equals(other); }

    /** @meta method []scala.Double; */
    public double  $plus      ()            { return +asDouble()        ; }
    /** @meta method []scala.Double; */
    public double  $minus     ()            { return -asDouble()        ; }



    public String  $plus      (String that) { return  asDouble() +  that; }

    public double  $plus      (double that) { return  asDouble() +  that; }
    public double  $minus     (double that) { return  asDouble() -  that; }
    public double  $times     (double that) { return  asDouble() *  that; }
    public double  $div       (double that) { return  asDouble() /  that; }
    public double  $percent   (double that) { return  asDouble() %  that; }
    public boolean $eq$eq     (double that) { return  asDouble() == that; }
    public boolean $bang$eq   (double that) { return  asDouble() != that; }
    public boolean $less      (double that) { return  asDouble() <  that; }
    public boolean $greater   (double that) { return  asDouble() >  that; }
    public boolean $less$eq   (double that) { return  asDouble() <= that; }
    public boolean $greater$eq(double that) { return  asDouble() >= that; }

}
