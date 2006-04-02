/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala;


/** @meta class extends scala.AnyVal; */
public abstract class Double extends AnyVal {

    // prevent inheriting from the class
    private Double() {}

    public boolean equals(java.lang.Object other) { return super.equals(other); }
    public int hashCode() { return super.hashCode(); }
    public String toString() { return super.toString(); }

    /** @meta method []scala.Byte; */
    abstract public byte toByte();

    /** @meta method []scala.Short; */
    abstract public short toShort();

    /** @meta method []scala.Char; */
    abstract public char toChar();

    /** @meta method []scala.Int; */
    abstract public int toInt();

    /** @meta method []scala.Long; */
    abstract public long toLong();

    /** @meta method []scala.Float; */
    abstract public float toFloat();

    /** @meta method []scala.Double; */
    abstract public double toDouble();

    /** @meta method (scala.Any)scala.Boolean; */
    abstract public boolean $eq$eq  (java.lang.Object other);
    /** @meta method (scala.Any)scala.Boolean; */
    abstract public boolean $bang$eq(java.lang.Object other);

    /** @meta method []scala.Double; */
    abstract public double  $plus();
    /** @meta method []scala.Double; */
    abstract public double  $minus();

    abstract public String  $plus      (String that);

    abstract public boolean $eq$eq     (double that);
    abstract public boolean $bang$eq   (double that);
    abstract public boolean $less      (double that);
    abstract public boolean $greater   (double that);
    abstract public boolean $less$eq   (double that);
    abstract public boolean $greater$eq(double that);
    abstract public double  $plus      (double that);
    abstract public double  $minus     (double that);
    abstract public double  $times     (double that);
    abstract public double  $div       (double that);
    abstract public double  $percent   (double that);

    abstract public boolean $eq$eq     (float  that);
    abstract public boolean $bang$eq   (float  that);
    abstract public boolean $less      (float  that);
    abstract public boolean $greater   (float  that);
    abstract public boolean $less$eq   (float  that);
    abstract public boolean $greater$eq(float  that);
    abstract public double  $plus      (float  that);
    abstract public double  $minus     (float  that);
    abstract public double  $times     (float  that);
    abstract public double  $div       (float  that);
    abstract public double  $percent   (float  that);

    abstract public boolean $eq$eq     (long   that);
    abstract public boolean $bang$eq   (long   that);
    abstract public boolean $less      (long   that);
    abstract public boolean $greater   (long   that);
    abstract public boolean $less$eq   (long   that);
    abstract public boolean $greater$eq(long   that);
    abstract public double  $plus      (long   that);
    abstract public double  $minus     (long   that);
    abstract public double  $times     (long   that);
    abstract public double  $div       (long   that);
    abstract public double  $percent   (long   that);

    abstract public boolean $eq$eq     (int    that);
    abstract public boolean $bang$eq   (int    that);
    abstract public boolean $less      (int    that);
    abstract public boolean $greater   (int    that);
    abstract public boolean $less$eq   (int    that);
    abstract public boolean $greater$eq(int    that);
    abstract public double  $plus      (int    that);
    abstract public double  $minus     (int    that);
    abstract public double  $times     (int    that);
    abstract public double  $div       (int    that);
    abstract public double  $percent   (int    that);

    abstract public boolean $eq$eq     (short  that);
    abstract public boolean $bang$eq   (short  that);
    abstract public boolean $less      (short  that);
    abstract public boolean $greater   (short  that);
    abstract public boolean $less$eq   (short  that);
    abstract public boolean $greater$eq(short  that);
    abstract public double  $plus      (short  that);
    abstract public double  $minus     (short  that);
    abstract public double  $times     (short  that);
    abstract public double  $div       (short  that);
    abstract public double  $percent   (short  that);

    abstract public boolean $eq$eq     (char   that);
    abstract public boolean $bang$eq   (char   that);
    abstract public boolean $less      (char   that);
    abstract public boolean $greater   (char   that);
    abstract public boolean $less$eq   (char   that);
    abstract public boolean $greater$eq(char   that);
    abstract public double  $plus      (char   that);
    abstract public double  $minus     (char   that);
    abstract public double  $times     (char   that);
    abstract public double  $div       (char   that);
    abstract public double  $percent   (char   that);

    abstract public boolean $eq$eq     (byte   that);
    abstract public boolean $bang$eq   (byte   that);
    abstract public boolean $less      (byte   that);
    abstract public boolean $greater   (byte   that);
    abstract public boolean $less$eq   (byte   that);
    abstract public boolean $greater$eq(byte   that);
    abstract public double  $plus      (byte   that);
    abstract public double  $minus     (byte   that);
    abstract public double  $times     (byte   that);
    abstract public double  $div       (byte   that);
    abstract public double  $percent   (byte   that);

}
