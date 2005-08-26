/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2005, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
package scala.runtime;

public class BoxedLong extends BoxedNumber {

    public static BoxedLong box(long value) {
	return new BoxedLong(value);
    }

    public final long value;

    private BoxedLong(long value) { this.value = value; }

    public final byte byteValue() { return (byte)value; }
    public final short shortValue() { return (short)value; }
    public final char charValue() { return (char)value; }
    public final int intValue() { return (int)value; }
    public final long longValue() { return (long)value; }
    public final float floatValue() { return (float)value; }
    public final double doubleValue() { return (double)value; }

    public boolean equals(java.lang.Object other) {
      return other instanceof BoxedNumber && value == ((BoxedNumber) other).longValue();
    }

    public int hashCode() {
	long bits = value;
	return (int)(bits ^ (bits >>> 32));
    }

    public String toString() {
      return String.valueOf(value);
    }
}
