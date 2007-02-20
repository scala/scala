/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.runtime;


public final class BoxedByte extends BoxedNumber
    implements java.io.Serializable
{

    private static final int MinHashed = -128;
    private static final int MaxHashed = 127;
    private static final BoxedByte[] canonical = new BoxedByte[MaxHashed - MinHashed + 1];

    static {
	for (int i = MinHashed; i <= MaxHashed; i++)
	    canonical[i - MinHashed] = new BoxedByte((byte)i);
    }

    public static BoxedByte box(byte value) {
	return canonical[value - MinHashed];
    }

    public final byte value;

    private BoxedByte(byte value) { this.value = value; }

    public byte byteValue() { return (byte)value; }
    public short shortValue() { return (short)value; }
    public char charValue() { return (char)value; }
    public int intValue() { return (int)value; }
    public long longValue() { return (long)value; }
    public float floatValue() { return (float)value; }
    public double doubleValue() { return (double)value; }

    public boolean equals(java.lang.Object other) {
	return other instanceof BoxedNumber &&
            value == ((BoxedNumber) other).byteValue();
    }

    public int hashCode() {
	return value;
    }

    public String toString() {
	return String.valueOf(value);
    }

}
