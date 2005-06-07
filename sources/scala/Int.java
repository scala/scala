/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala;

/** @meta class extends scala.AnyVal; */
public abstract class Int     extends AnyVal implements java.io.Serializable {

    public final int     value;

    public Int    (int     value) {
        this.value = value;
    }

    public boolean equals(java.lang.Object other) {
        return other instanceof Int     && value == ((Int    )other).value;
    }
    public int hashCode() {
        int  bits = value;
        return bits;
    }
    public String toString() {
        return String.valueOf(value);
    }

    /** @meta method (scala.Any)scala.Boolean; */
    public boolean $eq$eq  (java.lang.Object other) { return  equals(other); }
    /** @meta method (scala.Any)scala.Boolean; */
    public boolean $bang$eq(java.lang.Object other) { return !equals(other); }

    /** @meta method []scala.Int    ; */
    public int     $plus      (            ) { return +value        ; }

    /** @meta method []scala.Int    ; */
    public int     $minus     (            ) { return -value        ; }

    public String  $plus      (String  that) { return  value +  that; }

    public boolean $eq$eq     (double  that) { return  value == that; }
    public boolean $bang$eq   (double  that) { return  value != that; }
    public boolean $less      (double  that) { return  value <  that; }
    public boolean $greater   (double  that) { return  value >  that; }
    public boolean $less$eq   (double  that) { return  value <= that; }
    public boolean $greater$eq(double  that) { return  value >= that; }
    public double  $plus      (double  that) { return  value +  that; }
    public double  $minus     (double  that) { return  value -  that; }
    public double  $times     (double  that) { return  value *  that; }
    public double  $div       (double  that) { return  value /  that; }
    public double  $percent   (double  that) { return  value %  that; }

    /** @meta method []scala.Double ; */
    public double  coerce     (            ) { return  value        ; }

    public boolean $eq$eq     (float   that) { return  value == that; }
    public boolean $bang$eq   (float   that) { return  value != that; }
    public boolean $less      (float   that) { return  value <  that; }
    public boolean $greater   (float   that) { return  value >  that; }
    public boolean $less$eq   (float   that) { return  value <= that; }
    public boolean $greater$eq(float   that) { return  value >= that; }
    public float   $plus      (float   that) { return  value +  that; }
    public float   $minus     (float   that) { return  value -  that; }
    public float   $times     (float   that) { return  value *  that; }
    public float   $div       (float   that) { return  value /  that; }
    public float   $percent   (float   that) { return  value %  that; }

    /** @meta method []scala.Float  ; */
    public float   coerce     (            ) { return  value        ; }
    /** @meta method []scala.Int    ; */
    public int     $tilde     (            ) { return ~value        ; }

    public int     $less$less (int     that) { return  value << that; }
    public int     $less$less (long    that) { return  value << that; }
    public int     $greater$greater(int     that) { return  value >> that; }
    public int     $greater$greater(long    that) { return  value >> that; }
    public int     $greater$greater$greater(int     that) { return  value >>>that; }
    public int     $greater$greater$greater(long    that) { return  value >>>that; }

    public boolean $eq$eq     (long    that) { return  value == that; }
    public boolean $bang$eq   (long    that) { return  value != that; }
    public boolean $less      (long    that) { return  value <  that; }
    public boolean $greater   (long    that) { return  value >  that; }
    public boolean $less$eq   (long    that) { return  value <= that; }
    public boolean $greater$eq(long    that) { return  value >= that; }
    public long    $plus      (long    that) { return  value +  that; }
    public long    $minus     (long    that) { return  value -  that; }
    public long    $times     (long    that) { return  value *  that; }
    public long    $div       (long    that) { return  value /  that; }
    public long    $percent   (long    that) { return  value %  that; }
    public long    $bar       (long    that) { return  value |  that; }
    public long    $amp       (long    that) { return  value &  that; }
    public long    $up        (long    that) { return  value ^  that; }

    /** @meta method []scala.Long   ; */
    public long    coerce     (            ) { return  value        ; }

    public boolean $eq$eq     (int     that) { return  value == that; }
    public boolean $bang$eq   (int     that) { return  value != that; }
    public boolean $less      (int     that) { return  value <  that; }
    public boolean $greater   (int     that) { return  value >  that; }
    public boolean $less$eq   (int     that) { return  value <= that; }
    public boolean $greater$eq(int     that) { return  value >= that; }
    public int     $plus      (int     that) { return  value +  that; }
    public int     $minus     (int     that) { return  value -  that; }
    public int     $times     (int     that) { return  value *  that; }
    public int     $div       (int     that) { return  value /  that; }
    public int     $percent   (int     that) { return  value %  that; }
    public int     $bar       (int     that) { return  value |  that; }
    public int     $amp       (int     that) { return  value &  that; }
    public int     $up        (int     that) { return  value ^  that; }

}
