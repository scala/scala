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
public abstract class Float   extends AnyVal implements java.io.Serializable {

    public final float   value;

    public Float  (float   value) {
        this.value = value;
    }

    public boolean equals(java.lang.Object other) {
        return other instanceof Float   && value == ((Float  )other).value;
    }
    public int hashCode() {
        int  bits = java.lang.Float.floatToIntBits(value);
        return bits;
    }
    public String toString() {
        return String.valueOf(value);
    }

    /** @meta method []scala.Byte; */
    public byte toByte() { return (byte)value; }

    /** @meta method []scala.Short; */
    public short toShort() { return (short)value; }

    /** @meta method []scala.Char; */
    public char toChar() { return (char)value; }

    /** @meta method []scala.Int; */
    public int toInt() { return (int)value; }

    /** @meta method []scala.Long; */
    public long toLong() { return (long)value; }

    /** @meta method []scala.Float; */
    public float toFloat() { return (float)value; }

    /** @meta method []scala.Double; */
    public double toDouble() { return (double)value; }

    /** @meta method (scala.Any)scala.Boolean; */
    public boolean $eq$eq  (java.lang.Object other) { return  equals(other); }
    /** @meta method (scala.Any)scala.Boolean; */
    public boolean $bang$eq(java.lang.Object other) { return !equals(other); }

    /** @meta method []scala.Float  ; */
    public float   $plus      (            ) { return +value        ; }
    /** @meta method []scala.Float  ; */
    public float   $minus     (            ) { return -value        ; }

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

    public boolean $eq$eq     (long   that) { return  value == that; }
    public boolean $bang$eq   (long   that) { return  value != that; }
    public boolean $less      (long   that) { return  value <  that; }
    public boolean $greater   (long   that) { return  value >  that; }
    public boolean $less$eq   (long   that) { return  value <= that; }
    public boolean $greater$eq(long   that) { return  value >= that; }
    public float   $plus      (long   that) { return  value +  that; }
    public float   $minus     (long   that) { return  value -  that; }
    public float   $times     (long   that) { return  value *  that; }
    public float   $div       (long   that) { return  value /  that; }
    public float   $percent   (long   that) { return  value %  that; }

    public boolean $eq$eq     (int   that) { return  value == that; }
    public boolean $bang$eq   (int   that) { return  value != that; }
    public boolean $less      (int   that) { return  value <  that; }
    public boolean $greater   (int   that) { return  value >  that; }
    public boolean $less$eq   (int   that) { return  value <= that; }
    public boolean $greater$eq(int   that) { return  value >= that; }
    public float   $plus      (int   that) { return  value +  that; }
    public float   $minus     (int   that) { return  value -  that; }
    public float   $times     (int   that) { return  value *  that; }
    public float   $div       (int   that) { return  value /  that; }
    public float   $percent   (int   that) { return  value %  that; }

    public boolean $eq$eq     (short   that) { return  value == that; }
    public boolean $bang$eq   (short   that) { return  value != that; }
    public boolean $less      (short   that) { return  value <  that; }
    public boolean $greater   (short   that) { return  value >  that; }
    public boolean $less$eq   (short   that) { return  value <= that; }
    public boolean $greater$eq(short   that) { return  value >= that; }
    public float   $plus      (short   that) { return  value +  that; }
    public float   $minus     (short   that) { return  value -  that; }
    public float   $times     (short   that) { return  value *  that; }
    public float   $div       (short   that) { return  value /  that; }
    public float   $percent   (short   that) { return  value %  that; }

    public boolean $eq$eq     (char   that) { return  value == that; }
    public boolean $bang$eq   (char   that) { return  value != that; }
    public boolean $less      (char   that) { return  value <  that; }
    public boolean $greater   (char   that) { return  value >  that; }
    public boolean $less$eq   (char   that) { return  value <= that; }
    public boolean $greater$eq(char   that) { return  value >= that; }
    public float   $plus      (char   that) { return  value +  that; }
    public float   $minus     (char   that) { return  value -  that; }
    public float   $times     (char   that) { return  value *  that; }
    public float   $div       (char   that) { return  value /  that; }
    public float   $percent   (char   that) { return  value %  that; }

    public boolean $eq$eq     (byte   that) { return  value == that; }
    public boolean $bang$eq   (byte   that) { return  value != that; }
    public boolean $less      (byte   that) { return  value <  that; }
    public boolean $greater   (byte   that) { return  value >  that; }
    public boolean $less$eq   (byte   that) { return  value <= that; }
    public boolean $greater$eq(byte   that) { return  value >= that; }
    public float   $plus      (byte   that) { return  value +  that; }
    public float   $minus     (byte   that) { return  value -  that; }
    public float   $times     (byte   that) { return  value *  that; }
    public float   $div       (byte   that) { return  value /  that; }
    public float   $percent   (byte   that) { return  value %  that; }

}
