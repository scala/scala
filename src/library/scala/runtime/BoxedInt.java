/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.runtime;


public final class BoxedInt extends BoxedNumber
    implements java.io.Serializable
{

    private static final int MinHashed = -128;
    private static final int MaxHashed = 1024;
    private static final BoxedInt[] canonical = new BoxedInt[MaxHashed - MinHashed + 1];

    static {
	for (int i = MinHashed; i <= MaxHashed; i++)
	    canonical[i - MinHashed] = new BoxedInt(i);
    }

    public static BoxedInt box(int value) {
	if (MinHashed <= value && value <= MaxHashed) return canonical[value - MinHashed];
	else return new BoxedInt(value);
    }

    public final int value;

    private BoxedInt(int value) { this.value = value; }

    public final byte byteValue() { return (byte)value; }
    public final short shortValue() { return (short)value; }
    public final char charValue() { return (char)value; }
    public final int intValue() { return (int)value; }
    public final long longValue() { return (long)value; }
    public final float floatValue() { return (float)value; }
    public final double doubleValue() { return (double)value; }

    public final boolean equals(java.lang.Object other) {
	return other instanceof BoxedNumber &&
            value == ((BoxedNumber) other).intValue();
    }

    public final int hashCode() {
	return value;
    }

    public final String toString() {
	return String.valueOf(value);
    }
}
