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
public abstract class Int extends AnyVal {

    // prevent inheriting from the class
    private Int() {}

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

    /** @meta method []scala.Long; */
    abstract public long    coerce();
    /** @meta method []scala.Float; */
    abstract public float   coerce();
    /** @meta method []scala.Double; */
    abstract public double  coerce();

    /** @meta method []scala.Int; */
    abstract public int     $plus();
    /** @meta method []scala.Int; */
    abstract public int     $minus();
    /** @meta method []scala.Int; */
    abstract public int     $tilde();

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

    abstract public int     $less$less (int     that);
    abstract public int     $less$less (long    that);
    abstract public int     $greater$greater(int     that);
    abstract public int     $greater$greater(long    that);
    abstract public int     $greater$greater$greater(int     that);
    abstract public int     $greater$greater$greater(long    that);

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
    abstract public int     $plus      (int     that);
    abstract public int     $minus     (int     that);
    abstract public int     $times     (int     that);
    abstract public int     $div       (int     that);
    abstract public int     $percent   (int     that);
    abstract public int     $bar       (int     that);
    abstract public int     $amp       (int     that);
    abstract public int     $up        (int     that);


    abstract public boolean $eq$eq     (char    that);
    abstract public boolean $bang$eq   (char    that);
    abstract public boolean $less      (char    that);
    abstract public boolean $greater   (char    that);
    abstract public boolean $less$eq   (char    that);
    abstract public boolean $greater$eq(char    that);
    abstract public int     $plus      (char    that);
    abstract public int     $minus     (char    that);
    abstract public int     $times     (char    that);
    abstract public int     $div       (char    that);
    abstract public int     $percent   (char    that);
    abstract public int     $bar       (char    that);
    abstract public int     $amp       (char    that);
    abstract public int     $up        (char    that);

    abstract public boolean $eq$eq     (short   that);
    abstract public boolean $bang$eq   (short   that);
    abstract public boolean $less      (short   that);
    abstract public boolean $greater   (short   that);
    abstract public boolean $less$eq   (short   that);
    abstract public boolean $greater$eq(short   that);
    abstract public int     $plus      (short   that);
    abstract public int     $minus     (short   that);
    abstract public int     $times     (short   that);
    abstract public int     $div       (short   that);
    abstract public int     $percent   (short   that);
    abstract public int     $bar       (short   that);
    abstract public int     $amp       (short   that);
    abstract public int     $up        (short   that);

    abstract public boolean $eq$eq     (byte    that);
    abstract public boolean $bang$eq   (byte    that);
    abstract public boolean $less      (byte    that);
    abstract public boolean $greater   (byte    that);
    abstract public boolean $less$eq   (byte    that);
    abstract public boolean $greater$eq(byte    that);
    abstract public int     $plus      (byte    that);
    abstract public int     $minus     (byte    that);
    abstract public int     $times     (byte    that);
    abstract public int     $div       (byte    that);
    abstract public int     $percent   (byte    that);
    abstract public int     $bar       (byte    that);
    abstract public int     $amp       (byte    that);
    abstract public int     $up        (byte    that);

}
