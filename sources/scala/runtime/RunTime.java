/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $OldId: RunTime.java,v 1.13 2002/11/19 12:01:40 paltherr Exp $
// $Id$

package scala.runtime;

import scala.Unit;
import scala.Boolean;
import scala.Byte;
import scala.Short;
import scala.Char;
import scala.Int;
import scala.Long;
import scala.Float;
import scala.Double;
import scala.Array;
import java.lang.Object;

/**
 * Run-time support functions for Scala.
 */

public abstract class RunTime {
    public static final Unit UNIT_VAL = new Unit() {};

    private static ClassLoader loader = ClassLoader.getSystemClassLoader();

    private static final int BITS       = 8;
    private static final int ARRAY_SIZE = 2 << BITS;
    private static final int INDEX_MASK = ARRAY_SIZE - 1;
    private static final int CHECK_MASK = ~(ARRAY_SIZE / 2 - 1);

    private static final Boolean falseVal =
        new Boolean() { public boolean asBoolean() { return false; } };
    private static final Boolean trueVal =
        new Boolean() { public boolean asBoolean() { return true ; } };

    private static final Byte [] bval = new Byte [256];
    private static final Short[] sval = new Short[ARRAY_SIZE];
    private static final Char [] cval = new Char [ARRAY_SIZE / 2];
    private static final Int  [] ival = new Int  [ARRAY_SIZE];
    private static final Long [] lval = new Long [ARRAY_SIZE];

    static {
        for (int i = 0; i < bval.length; i++) bval[i] = box0((byte)i);
        for (int i = 0; i < ARRAY_SIZE / 2; i++) {
            sval[i] = box0((short)i);
            cval[i] = box0((char )i);
            ival[i] = box0((int  )i);
            lval[i] = box0((long )i);
            sval[i + ARRAY_SIZE / 2] = box0((short)(CHECK_MASK | i));
            ival[i + ARRAY_SIZE / 2] = box0((int  )(CHECK_MASK | i));
            lval[i + ARRAY_SIZE / 2] = box0((long )(CHECK_MASK | i));
        }
    }

    public static ClassLoader getClassLoader() {
        return loader;
    }

    public static void setClassLoader(ClassLoader loader) {
        RunTime.loader = loader;
    }

    public static Unit box() {
	return UNIT_VAL;
    }

    public static Boolean box(final boolean x) {
        return x ? trueVal : falseVal;
    }

    public static Byte box(final byte x) {
        return bval[x & 0x000000FF];
    }

    private static Byte box0(final byte x) {
        return new Byte() {
	    public double asDouble() { return (double)x; }
	    public float asFloat() { return (float)x; }
	    public long asLong() { return (long)x; }
	    public int asInt() { return (int)x; }
	    public char asChar() { return (char)x; }
	    public short asShort() { return (short)x; }
	    public byte asByte() { return (byte)x; }

	};
    }

    public static Short box(final short x) {
        int c = x & CHECK_MASK;
        return c == 0 || c == CHECK_MASK ? sval[x & INDEX_MASK] : box0(x);
    }

    private static Short box0(final short x) {
	return new Short() {
            public double asDouble() { return (double)x; }
            public float asFloat() { return (float)x; }
            public long asLong() { return (long)x; }
            public int asInt() { return (int)x; }
            public char asChar() {
                if (isChar()) return (char)x;
                else throw new ClassCastException();
            }
            public short asShort() { return (short)x; }
            public byte asByte() {
                if (isByte()) return (byte)x;
                else throw new ClassCastException();
            }

	    public boolean isChar() {
                return (java.lang.Character.MIN_VALUE <= x)
                    && (java.lang.Character.MAX_VALUE >= x);
            }
	    public boolean isByte() {
                return (java.lang.Byte.MIN_VALUE <= x)
                    && (java.lang.Byte.MAX_VALUE >= x);
            }
        };
    }

    public static Char box(final char x) {
        int c = (int)x & CHECK_MASK;
        return c == 0 ? cval[(int)x & INDEX_MASK] : box0(x);
    }

    private static Char box0(final char x) {
        return new Char() {
            public double asDouble() { return (double)x; }
            public float asFloat() { return (float)x; }
            public long asLong() { return (long)x; }
            public int asInt() { return (int)x; }
            public char asChar() { return (char)x; }
            public short asShort() {
                if (isShort()) return (short)x;
                else throw new ClassCastException();
            }
            public byte asByte() {
                if (isByte()) return (byte)x;
                else throw new ClassCastException();
            }

	    public boolean isShort() {
                return (java.lang.Short.MIN_VALUE <= x)
                    && (java.lang.Short.MAX_VALUE >= x);
            }
	    public boolean isByte() {
                return (java.lang.Byte.MIN_VALUE <= x)
                    && (java.lang.Byte.MAX_VALUE >= x);
            }
        };
    }

    public static Int box(final int x) {
        int c = x & CHECK_MASK;
        return c == 0 || c == CHECK_MASK ? ival[x & INDEX_MASK] : box0(x);
    }

    private static Int box0(final int x) {
        return new Int() {
            public double asDouble() { return (double)x; }
            public float asFloat() { return (float)x; }
            public long asLong() { return (long)x; }
            public int asInt() { return (int)x; }
            public char asChar() {
                if (isChar()) return (char)x;
                else throw new ClassCastException();
            }
            public short asShort() {
                if (isShort()) return (short)x;
                else throw new ClassCastException();
            }
            public byte asByte() {
                if (isByte()) return (byte)x;
                else throw new ClassCastException();
            }

	    public boolean isChar() {
                return (java.lang.Character.MIN_VALUE <= x)
                    && (java.lang.Character.MAX_VALUE >= x);
            }
	    public boolean isShort() {
                return (java.lang.Short.MIN_VALUE <= x)
                    && (java.lang.Short.MAX_VALUE >= x);
            }
	    public boolean isByte() {
                return (java.lang.Byte.MIN_VALUE <= x)
                    && (java.lang.Byte.MAX_VALUE >= x);
            }
	};
    }

    public static Long box(final long x) {
        long c = x & CHECK_MASK;
        return c == 0 || c == CHECK_MASK ? lval[(int)x & INDEX_MASK] : box0(x);
    }

    private static Long box0(final long x) {
        return new Long() {
            public double asDouble() { return (double)x; }
            public float asFloat() { return (float)x; }
            public long asLong() { return (long)x; }
            public int asInt() {
                if (isInt()) return (int)x;
                else throw new ClassCastException();
            }
            public char asChar() {
                if (isChar()) return (char)x;
                else throw new ClassCastException();
            }
            public short asShort() {
                if (isShort()) return (short)x;
                else throw new ClassCastException();
            }
            public byte asByte() {
                if (isByte()) return (byte)x;
                else throw new ClassCastException();
            }

	    public boolean isInt() {
                return (java.lang.Integer.MIN_VALUE <= x)
                    && (java.lang.Integer.MAX_VALUE >= x);
            }
	    public boolean isChar() {
                return (java.lang.Character.MIN_VALUE <= x)
                    && (java.lang.Character.MAX_VALUE >= x);
            }
	    public boolean isShort() {
                return (java.lang.Short.MIN_VALUE <= x)
                    && (java.lang.Short.MAX_VALUE >= x);
            }
	    public boolean isByte() {
                return (java.lang.Byte.MIN_VALUE <= x)
                    && (java.lang.Byte.MAX_VALUE >= x);
            }
        };
    }

    public static Float box(final float x) {
        return new Float() {
            public double asDouble() { return (double)x; }
            public float asFloat() { return (float)x; }
            public long asLong() {
                if (isLong()) return (long)x;
                else throw new ClassCastException();
            }
            public int asInt() {
                if (isInt()) return (int)x;
                else throw new ClassCastException();
            }
            public char asChar() {
                if (isChar()) return (char)x;
                else throw new ClassCastException();
            }
            public short asShort() {
                if (isShort()) return (short)x;
                else throw new ClassCastException();
            }
            public byte asByte() {
                if (isByte()) return (byte)x;
                else throw new ClassCastException();
            }

	    public boolean isLong() {
                return (java.lang.Long.MIN_VALUE <= x)
                    && (java.lang.Long.MAX_VALUE >= x);
            }
	    public boolean isInt() {
                return (java.lang.Integer.MIN_VALUE <= x)
                    && (java.lang.Integer.MAX_VALUE >= x);
            }
	    public boolean isChar() {
                return (java.lang.Character.MIN_VALUE <= x)
                    && (java.lang.Character.MAX_VALUE >= x);
            }
	    public boolean isShort() {
                return (java.lang.Short.MIN_VALUE <= x)
                    && (java.lang.Short.MAX_VALUE >= x);
            }
	    public boolean isByte() {
                return (java.lang.Byte.MIN_VALUE <= x)
                    && (java.lang.Byte.MAX_VALUE >= x);
            }
        };
    }

    public static Double box(final double x) {
        return new Double() {
            public double asDouble() { return (double)x; }
            public float asFloat() {
                if (isFloat()) return (float)x;
                else throw new ClassCastException();
            }
            public long asLong() {
                if (isLong()) return (long)x;
                else throw new ClassCastException();
            }
            public int asInt() {
                if (isInt()) return (int)x;
                else throw new ClassCastException();
            }
            public char asChar() {
                if (isChar()) return (char)x;
                else throw new ClassCastException();
            }
            public short asShort() {
                if (isShort()) return (short)x;
                else throw new ClassCastException();
            }
            public byte asByte() {
                if (isByte()) return (byte)x;
                else throw new ClassCastException();
            }

	    public boolean isFloat() {
                return (java.lang.Float.MIN_VALUE <= x)
                    && (java.lang.Float.MAX_VALUE >= x);
            }
	    public boolean isLong() {
                return (java.lang.Long.MIN_VALUE <= x)
                    && (java.lang.Long.MAX_VALUE >= x);
            }
	    public boolean isInt() {
                return (java.lang.Integer.MIN_VALUE <= x)
                    && (java.lang.Integer.MAX_VALUE >= x);
            }
	    public boolean isChar() {
                return (java.lang.Character.MIN_VALUE <= x)
                    && (java.lang.Character.MAX_VALUE >= x);
            }
	    public boolean isShort() {
                return (java.lang.Short.MIN_VALUE <= x)
                    && (java.lang.Short.MAX_VALUE >= x);
            }
	    public boolean isByte() {
                return (java.lang.Byte.MIN_VALUE <= x)
                    && (java.lang.Byte.MAX_VALUE >= x);
            }
        };
    }

    public static Array box(final boolean[] xs) {
	return new Array(0) {
	    public boolean[] asBooleanArray() { return xs; }
	    public Object apply(int i) { return box(xs[i]); }
	    public void update(int i, Object x) { xs[i] = ((Boolean)x).asBoolean(); }
	    public int length() { return xs.length; }
            public String toString() { return String.valueOf(xs); }
	};
    }

    public static Array box(final byte[] xs) {
	return new Array(0) {
	    public byte[] asByteArray() { return xs; }
	    public Object apply(int i) { return box(xs[i]); }
	    public void update(int i, Object x) { xs[i] = ((Byte)x).asByte(); }
	    public int length() { return xs.length; }
            public String toString() { return String.valueOf(xs); }
	};
    }

    public static Array box(final short[] xs) {
	return new Array(0) {
	    public short[] asShortArray() { return xs; }
	    public Object apply(int i) { return box(xs[i]); }
	    public void update(int i, Object x) { xs[i] = ((Short)x).asShort(); }
	    public int length() { return xs.length; }
            public String toString() { return String.valueOf(xs); }
	};
    }

    public static Array box(final char[] xs) {
	return new Array(0) {
	    public char[] asCharArray() { return xs; }
	    public Object apply(int i) { return box(xs[i]); }
	    public void update(int i, Object x) { xs[i] = ((Char)x).asChar(); }
	    public int length() { return xs.length; }
            public String toString() { return String.valueOf((Object)xs); }
	};
    }

    public static Array box(final int[] xs) {
	return new Array(0) {
	    public int[] asIntArray() { return xs; }
	    public Object apply(int i) { return box(xs[i]); }
	    public void update(int i, Object x) { xs[i] = ((Int)x).asInt(); }
	    public int length() { return xs.length; }
            public String toString() { return String.valueOf(xs); }
	};
    }

    public static Array box(final long[] xs) {
	return new Array(0) {
	    public long[] asLongArray() { return xs; }
	    public Object apply(int i) { return box(xs[i]); }
	    public void update(int i, Object x) { xs[i] = ((Long)x).asLong(); }
	    public int length() { return xs.length; }
            public String toString() { return String.valueOf(xs); }
	};
    }

    public static Array box(final float[] xs) {
	return new Array(0) {
	    public float[] asFloatArray() { return xs; }
	    public Object apply(int i) { return box(xs[i]); }
	    public void update(int i, Object x) { xs[i] = ((Float)x).asFloat(); }
	    public int length() { return xs.length; }
            public String toString() { return String.valueOf(xs); }
	};
    }

    public static Array box(final double[] xs) {
	return new Array(0) {
	    public double[] asDoubleArray() { return xs; }
	    public Object apply(int i) { return box(xs[i]); }
	    public void update(int i, Object x) { xs[i] = ((Double)x).asDouble(); }
	    public int length() { return xs.length; }
            public String toString() { return String.valueOf(xs); }
	};
    }

    public static Array box(final Object[] xs) {
	return new Array(0) {
	    public Object[] asObjectArray() { return xs; }
	    public Object apply(int i) { return xs[i]; }
	    public void update(int i, Object x) { xs[i] = x; }
	    public int length() { return xs.length; }
            public String toString() { return String.valueOf(xs); }
	};
    }

    public static boolean[] zarray(int length) {
        return new boolean[length];
    }

    public static byte[] barray(int length) {
        return new byte[length];
    }

    public static short[] sarray(int length) {
        return new short[length];
    }

    public static char[] carray(int length) {
        return new char[length];
    }

    public static int[] iarray(int length) {
        return new int[length];
    }

    public static long[] larray(int length) {
        return new long[length];
    }

    public static float[] farray(int length) {
        return new float[length];
    }

    public static double[] darray(int length) {
        return new double[length];
    }

    public static Object oarray(int length, String classname) {
        try {
            Class clasz = Class.forName(classname, false, loader);
            return java.lang.reflect.Array.newInstance(clasz, length);
        } catch (ClassNotFoundException exception) {
            throw new Error(exception.toString());
        }
    }

    public static boolean zarray_get(boolean[] array, int index) {
        return array[index];
    }

    public static byte barray_get(byte[] array, int index) {
        return array[index];
    }

    public static short sarray_get(short[] array, int index) {
        return array[index];
    }

    public static char carray_get(char[] array, int index) {
        return array[index];
    }

    public static int iarray_get(int[] array, int index) {
        return array[index];
    }

    public static long larray_get(long[] array, int index) {
        return array[index];
    }

    public static float farray_get(float[] array, int index) {
        return array[index];
    }

    public static double darray_get(double[] array, int index) {
        return array[index];
    }

    public static Object oarray_get(Object[] array, int index) {
        return array[index];
    }

    public static void zarray_set(boolean[] array, int index, boolean value) {
        array[index] = value;
    }

    public static void barray_set(byte[] array, int index, byte value) {
        array[index] = value;
    }

    public static void sarray_set(short[] array, int index, short value) {
        array[index] = value;
    }

    public static void carray_set(char[] array, int index, char value) {
        array[index] = value;
    }

    public static void iarray_set(int[] array, int index, int value) {
        array[index] = value;
    }

    public static void larray_set(long[] array, int index, long value) {
        array[index] = value;
    }

    public static void farray_set(float[] array, int index, float value) {
        array[index] = value;
    }

    public static void darray_set(double[] array, int index, double value) {
        array[index] = value;
    }

    public static void oarray_set(Object[] array, int index, Object value) {
        array[index] = value;
    }

}
