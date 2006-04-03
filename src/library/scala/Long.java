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
public abstract class Long extends AnyVal {

    // prevent inheriting from the class
    private Long() {}

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
    abstract public float   coerce();

    /** @meta method []scala.Long; */
    abstract public long     $plus();
    /** @meta method []scala.Long; */
    abstract public long     $minus();
    /** @meta method []scala.Long; */
    abstract public long     $tilde();

    abstract public String  $plus      (String  that);

    abstract public boolean $eq$eq     (double  that);
    abstract public boolean $bang$eq   (double  that);
    abstract public boolean $less      (double  that);
    abstract public boolean $greater   (double  that);
    abstract public boolean $less$eq   (double  that);
    abstract public boolean $greater$eq(double  that);
    abstract public double  $plus      (double  that);
    abstract public double  $minus     (double  that);
    abstract public double  $times     (double  that);
    abstract public double  $div       (double  that);
    abstract public double  $percent   (double  that);

    abstract public boolean $eq$eq     (float   that);
    abstract public boolean $bang$eq   (float   that);
    abstract public boolean $less      (float   that);
    abstract public boolean $greater   (float   that);
    abstract public boolean $less$eq   (float   that);
    abstract public boolean $greater$eq(float   that);
    abstract public float   $plus      (float   that);
    abstract public float   $minus     (float   that);
    abstract public float   $times     (float   that);
    abstract public float   $div       (float   that);
    abstract public float   $percent   (float   that);

    abstract public long    $less$less (int     that);
    abstract public long    $less$less (long    that);
    abstract public long    $greater$greater(int     that);
    abstract public long    $greater$greater(long    that);
    abstract public long    $greater$greater$greater(int     that);
    abstract public long    $greater$greater$greater(long    that);

    abstract public boolean $eq$eq     (long    that);
    abstract public boolean $bang$eq   (long    that);
    abstract public boolean $less      (long    that);
    abstract public boolean $greater   (long    that);
    abstract public boolean $less$eq   (long    that);
    abstract public boolean $greater$eq(long    that);
    abstract public long    $plus      (long    that);
    abstract public long    $minus     (long    that);
    abstract public long    $times     (long    that);
    abstract public long    $div       (long    that);
    abstract public long    $percent   (long    that);
    abstract public long    $bar       (long    that);
    abstract public long    $amp       (long    that);
    abstract public long    $up        (long    that);

    abstract public boolean $eq$eq     (int     that);
    abstract public boolean $bang$eq   (int     that);
    abstract public boolean $less      (int     that);
    abstract public boolean $greater   (int     that);
    abstract public boolean $less$eq   (int     that);
    abstract public boolean $greater$eq(int     that);
    abstract public long    $plus      (int     that);
    abstract public long    $minus     (int     that);
    abstract public long    $times     (int     that);
    abstract public long    $div       (int     that);
    abstract public long    $percent   (int     that);
    abstract public long    $bar       (int     that);
    abstract public long    $amp       (int     that);
    abstract public long    $up        (int     that);

    abstract public boolean $eq$eq     (char    that);
    abstract public boolean $bang$eq   (char    that);
    abstract public boolean $less      (char    that);
    abstract public boolean $greater   (char    that);
    abstract public boolean $less$eq   (char    that);
    abstract public boolean $greater$eq(char    that);
    abstract public long    $plus      (char    that);
    abstract public long    $minus     (char    that);
    abstract public long    $times     (char    that);
    abstract public long    $div       (char    that);
    abstract public long    $percent   (char    that);
    abstract public long    $bar       (char    that);
    abstract public long    $amp       (char    that);
    abstract public long    $up        (char    that);

    abstract public boolean $eq$eq     (short   that);
    abstract public boolean $bang$eq   (short   that);
    abstract public boolean $less      (short   that);
    abstract public boolean $greater   (short   that);
    abstract public boolean $less$eq   (short   that);
    abstract public boolean $greater$eq(short   that);
    abstract public long    $plus      (short   that);
    abstract public long    $minus     (short   that);
    abstract public long    $times     (short   that);
    abstract public long    $div       (short   that);
    abstract public long    $percent   (short   that);
    abstract public long    $bar       (short   that);
    abstract public long    $amp       (short   that);
    abstract public long    $up        (short   that);

    abstract public boolean $eq$eq     (byte    that);
    abstract public boolean $bang$eq   (byte    that);
    abstract public boolean $less      (byte    that);
    abstract public boolean $greater   (byte    that);
    abstract public boolean $less$eq   (byte    that);
    abstract public boolean $greater$eq(byte    that);
    abstract public long    $plus      (byte    that);
    abstract public long    $minus     (byte    that);
    abstract public long    $times     (byte    that);
    abstract public long    $div       (byte    that);
    abstract public long    $percent   (byte    that);
    abstract public long    $bar       (byte    that);
    abstract public long    $amp       (byte    that);
    abstract public long    $up        (byte    that);

}
