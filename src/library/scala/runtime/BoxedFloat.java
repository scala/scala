/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.runtime;


public class BoxedFloat extends BoxedNumber
    implements java.io.Serializable
{

    public static BoxedFloat box(float value) {
	return new BoxedFloat(value);
    }

    public final float value;

    private BoxedFloat(float value) { this.value = value; }

    public final byte byteValue() { return (byte)value; }
    public final short shortValue() { return (short)value; }
    public final char charValue() { return (char)value; }
    public final int intValue() { return (int)value; }
    public final long longValue() { return (long)value; }
    public final float floatValue() { return (float)value; }
    public final double doubleValue() { return (double)value; }

    public boolean equals(java.lang.Object other) {
	return other instanceof BoxedNumber &&
            value == ((BoxedNumber) other).floatValue();
    }

    public int hashCode() {
	return java.lang.Float.floatToIntBits(value);
    }

    public String toString() {
	return String.valueOf(value);
    }

}
