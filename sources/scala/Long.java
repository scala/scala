/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala;

public abstract class Long   extends AnyVal {



    /** @meta method []scala.Float ; */
    public float  coerce() { return asFloat (); }
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
        return other instanceof Long   && $eq$eq(((Long  )other).asLong  ());
    }
    public int hashCode() {
        long bits = asLong();
        return (int)(bits ^ (bits >>> 32));
    }
    public String toString() {
        return String.valueOf(asLong  ());
    }

    /** @meta method (scala.Any)scala.Boolean; */
    public boolean $eq$eq  (java.lang.Object other) { return  equals(other); }
    /** @meta method (scala.Any)scala.Boolean; */
    public boolean $bang$eq(java.lang.Object other) { return !equals(other); }

    /** @meta method []scala.Long  ; */
    public long    $plus      ()            { return +asLong  ()        ; }
    /** @meta method []scala.Long  ; */
    public long    $minus     ()            { return -asLong  ()        ; }
    /** @meta method []scala.Long  ; */
    public long    $tilde     ()            { return ~asLong  ()        ; }

    public String  $plus      (String that) { return  asLong  () +  that; }

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

    public long    $less$less (int    that) { return  asLong  () << that; }
    public long    $less$less (long   that) { return  asLong  () << that; }
    public long    $greater$greater(int    that) { return  asLong  () >> that; }
    public long    $greater$greater(long   that) { return  asLong  () >> that; }
    public long    $greater$greater$greater(int    that) { return  asLong  () >>>that; }
    public long    $greater$greater$greater(long   that) { return  asLong  () >>>that; }

    public long    $amp       (long   that) { return  asLong  () &  that; }
    public long    $bar       (long   that) { return  asLong  () |  that; }
    public long    $up        (long   that) { return  asLong  () ^  that; }
    public long    $plus      (long   that) { return  asLong  () +  that; }
    public long    $minus     (long   that) { return  asLong  () -  that; }
    public long    $times     (long   that) { return  asLong  () *  that; }
    public long    $div       (long   that) { return  asLong  () /  that; }
    public long    $percent   (long   that) { return  asLong  () %  that; }
    public boolean $eq$eq     (long   that) { return  asLong  () == that; }
    public boolean $bang$eq   (long   that) { return  asLong  () != that; }
    public boolean $less      (long   that) { return  asLong  () <  that; }
    public boolean $greater   (long   that) { return  asLong  () >  that; }
    public boolean $less$eq   (long   that) { return  asLong  () <= that; }
    public boolean $greater$eq(long   that) { return  asLong  () >= that; }

}
