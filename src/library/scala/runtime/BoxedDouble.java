/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.runtime;


public class BoxedDouble extends BoxedNumber
    implements java.io.Serializable
{

    public static BoxedDouble box(double value) {
	return new BoxedDouble(value);
    }

    public final double value;

    private BoxedDouble(double value) { this.value = value; }

    public final byte byteValue() { return (byte)value; }
    public final short shortValue() { return (short)value; }
    public final char charValue() { return (char)value; }
    public final int intValue() { return (int)value; }
    public final long longValue() { return (long)value; }
    public final float floatValue() { return (float)value; }
    public final double doubleValue() { return (double)value; }

    public boolean equals(java.lang.Object other) {
	return other instanceof BoxedNumber &&
            value == ((BoxedNumber) other).doubleValue();
    }

    public int hashCode() {
	long bits = java.lang.Double.doubleToLongBits(value);
	return (int)(bits ^ (bits >>> 32));
    }

    public String toString() {
	return String.valueOf(value);
    }

}
