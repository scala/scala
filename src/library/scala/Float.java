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
public abstract class Float extends AnyVal {

    // prevent inheriting from the class
    private Float() {}

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

    /** @meta method []scala.Double; */
    abstract public double  coerce();

    /** @meta method []scala.Float; */
    abstract public float   $plus      ();
    /** @meta method []scala.Float; */
    abstract public float   $minus     ();

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
    abstract public float   $plus      (float  that);
    abstract public float   $minus     (float  that);
    abstract public float   $times     (float  that);
    abstract public float   $div       (float  that);
    abstract public float   $percent   (float  that);

    abstract public boolean $eq$eq     (long   that);
    abstract public boolean $bang$eq   (long   that);
    abstract public boolean $less      (long   that);
    abstract public boolean $greater   (long   that);
    abstract public boolean $less$eq   (long   that);
    abstract public boolean $greater$eq(long   that);
    abstract public float   $plus      (long   that);
    abstract public float   $minus     (long   that);
    abstract public float   $times     (long   that);
    abstract public float   $div       (long   that);
    abstract public float   $percent   (long   that);

    abstract public boolean $eq$eq     (int    that);
    abstract public boolean $bang$eq   (int    that);
    abstract public boolean $less      (int    that);
    abstract public boolean $greater   (int    that);
    abstract public boolean $less$eq   (int    that);
    abstract public boolean $greater$eq(int    that);
    abstract public float   $plus      (int    that);
    abstract public float   $minus     (int    that);
    abstract public float   $times     (int    that);
    abstract public float   $div       (int    that);
    abstract public float   $percent   (int    that);

    abstract public boolean $eq$eq     (short  that);
    abstract public boolean $bang$eq   (short  that);
    abstract public boolean $less      (short  that);
    abstract public boolean $greater   (short  that);
    abstract public boolean $less$eq   (short  that);
    abstract public boolean $greater$eq(short  that);
    abstract public float   $plus      (short  that);
    abstract public float   $minus     (short  that);
    abstract public float   $times     (short  that);
    abstract public float   $div       (short  that);
    abstract public float   $percent   (short  that);

    abstract public boolean $eq$eq     (char   that);
    abstract public boolean $bang$eq   (char   that);
    abstract public boolean $less      (char   that);
    abstract public boolean $greater   (char   that);
    abstract public boolean $less$eq   (char   that);
    abstract public boolean $greater$eq(char   that);
    abstract public float   $plus      (char   that);
    abstract public float   $minus     (char   that);
    abstract public float   $times     (char   that);
    abstract public float   $div       (char   that);
    abstract public float   $percent   (char   that);

    abstract public boolean $eq$eq     (byte   that);
    abstract public boolean $bang$eq   (byte   that);
    abstract public boolean $less      (byte   that);
    abstract public boolean $greater   (byte   that);
    abstract public boolean $less$eq   (byte   that);
    abstract public boolean $greater$eq(byte   that);
    abstract public float   $plus      (byte   that);
    abstract public float   $minus     (byte   that);
    abstract public float   $times     (byte   that);
    abstract public float   $div       (byte   that);
    abstract public float   $percent   (byte   that);

}
