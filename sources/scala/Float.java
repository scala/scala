/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala;

public abstract class Float  extends AnyVal {









    /** @meta method []scala.Double; */
    public double coerce() { return asDouble(); }

    public abstract double asDouble();
    public abstract float  asFloat();
    public abstract long   asLong();
    public abstract int    asInt();
    public abstract char   asChar();
    public abstract short  asShort();
    public abstract byte   asByte();

    public boolean equals(java.lang.Object other) {
        return other instanceof Float  && $eq$eq(((Float )other).asFloat ());
    }
    public int hashCode() {
        int  bits = java.lang.Float.floatToIntBits(asFloat());
        return bits;
    }
    public String toString() {
        return String.valueOf(asFloat ());
    }

    /** @meta method (scala.Any)scala.Boolean; */
    public boolean $eq$eq  (java.lang.Object other) { return  equals(other); }
    /** @meta method (scala.Any)scala.Boolean; */
    public boolean $bang$eq(java.lang.Object other) { return !equals(other); }

    /** @meta method []scala.Float ; */
    public float   $plus      ()            { return +asFloat ()        ; }
    /** @meta method []scala.Float ; */
    public float   $minus     ()            { return -asFloat ()        ; }



    public String  $plus      (String that) { return  asFloat () +  that; }

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

    public float   $plus      (float  that) { return  asFloat () +  that; }
    public float   $minus     (float  that) { return  asFloat () -  that; }
    public float   $times     (float  that) { return  asFloat () *  that; }
    public float   $div       (float  that) { return  asFloat () /  that; }
    public float   $percent   (float  that) { return  asFloat () %  that; }
    public boolean $eq$eq     (float  that) { return  asFloat () == that; }
    public boolean $bang$eq   (float  that) { return  asFloat () != that; }
    public boolean $less      (float  that) { return  asFloat () <  that; }
    public boolean $greater   (float  that) { return  asFloat () >  that; }
    public boolean $less$eq   (float  that) { return  asFloat () <= that; }
    public boolean $greater$eq(float  that) { return  asFloat () >= that; }

}
