/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala;

public abstract class Int    extends AnyVal {





    /** @meta method []scala.Long  ; */
    public long   coerce() { return asLong  (); }
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
        return other instanceof Int    && $eq$eq(((Int   )other).asInt   ());
    }
    public int hashCode() {
        int  bits = asInt   ();
        return bits;
    }
    public String toString() {
        return String.valueOf(asInt   ());
    }

    /** @meta method (scala.Any)scala.Boolean; */
    public boolean $eq$eq  (java.lang.Object other) { return  equals(other); }
    /** @meta method (scala.Any)scala.Boolean; */
    public boolean $bang$eq(java.lang.Object other) { return !equals(other); }

    /** @meta method []scala.Int   ; */
    public int     $plus      ()            { return +asInt   ()        ; }
    /** @meta method []scala.Int   ; */
    public int     $minus     ()            { return -asInt   ()        ; }
    /** @meta method []scala.Int   ; */
    public int     $tilde     ()            { return ~asInt   ()        ; }

    public String  $plus      (String that) { return  asInt   () +  that; }

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

    public int     $less$less (int    that) { return  asInt   () << that; }
    public int     $less$less (long   that) { return  asInt   () << that; }
    public int     $greater$greater(int    that) { return  asInt   () >> that; }
    public int     $greater$greater(long   that) { return  asInt   () >> that; }
    public int     $greater$greater$greater(int    that) { return  asInt   () >>>that; }
    public int     $greater$greater$greater(long   that) { return  asInt   () >>>that; }

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

    public int     $amp       (int    that) { return  asInt   () &  that; }
    public int     $bar       (int    that) { return  asInt   () |  that; }
    public int     $up        (int    that) { return  asInt   () ^  that; }
    public int     $plus      (int    that) { return  asInt   () +  that; }
    public int     $minus     (int    that) { return  asInt   () -  that; }
    public int     $times     (int    that) { return  asInt   () *  that; }
    public int     $div       (int    that) { return  asInt   () /  that; }
    public int     $percent   (int    that) { return  asInt   () %  that; }
    public boolean $eq$eq     (int    that) { return  asInt   () == that; }
    public boolean $bang$eq   (int    that) { return  asInt   () != that; }
    public boolean $less      (int    that) { return  asInt   () <  that; }
    public boolean $greater   (int    that) { return  asInt   () >  that; }
    public boolean $less$eq   (int    that) { return  asInt   () <= that; }
    public boolean $greater$eq(int    that) { return  asInt   () >= that; }

}
