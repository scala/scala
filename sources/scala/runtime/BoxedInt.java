/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2005, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
package scala.runtime;

public final class BoxedInt extends BoxedNumber {

    private static final int MinHashed = -128;
    private static final int MaxHashed = 1024;
    private static BoxedInt[] canonical = new BoxedInt[MaxHashed - MinHashed + 1];

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

    public byte byteValue() { return (byte)value; }
    public short shortValue() { return (short)value; }
    public char charValue() { return (char)value; }
    public int intValue() { return (int)value; }
    public long longValue() { return (long)value; }
    public float floatValue() { return (float)value; }
    public double doubleValue() { return (double)value; }

    public boolean equals(java.lang.Object other) {
	return other instanceof BoxedNumber && value == ((BoxedNumber) other).intValue();
    }

    public int hashCode() {
	return value;
    }

    public String toString() {
	return String.valueOf(value);
    }
}
