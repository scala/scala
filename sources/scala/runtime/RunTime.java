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

public abstract class RunTime {

    //########################################################################
    // Private Constants

    private static final int BITS       = 8;
    private static final int ARRAY_SIZE = 2 << BITS;
    private static final int INDEX_MASK = ARRAY_SIZE - 1;
    private static final int CHECK_MASK = ~(ARRAY_SIZE / 2 - 1);

    private static final UValue uvalue   = new UValue();
    private static final ZValue zvalue_f = new ZValue(false);
    private static final ZValue zvalue_t = new ZValue(true);
    private static final BValue bvalue[] = new BValue[256];
    private static final SValue svalue[] = new SValue[ARRAY_SIZE];
    private static final CValue cvalue[] = new CValue[ARRAY_SIZE / 2];
    private static final IValue ivalue[] = new IValue[ARRAY_SIZE];
    private static final LValue lvalue[] = new LValue[ARRAY_SIZE];

    static {
        for (int i = 0; i < bvalue.length; i++)
            bvalue[i] = new BValue((byte)i);
        for (int i = 0; i < ARRAY_SIZE / 2; i++) {
            svalue[i] = new SValue((short)i);
            cvalue[i] = new CValue((char )i);
            ivalue[i] = new IValue((int  )i);
            lvalue[i] = new LValue((long )i);
            svalue[i + ARRAY_SIZE / 2] = new SValue((short)(CHECK_MASK | i));
            ivalue[i + ARRAY_SIZE / 2] = new IValue((int  )(CHECK_MASK | i));
            lvalue[i + ARRAY_SIZE / 2] = new LValue((long )(CHECK_MASK | i));
        }
    }

    //########################################################################
    // Private Variables

    private static ClassLoader loader = ClassLoader.getSystemClassLoader();

    //########################################################################
    // Public Functions - Getting & setting class loader

    public static ClassLoader getClassLoader() {
        return loader;
    }

    public static void setClassLoader(ClassLoader loader) {
        RunTime.loader = loader;
    }

    //########################################################################
    // Public Functions - Boxing primitives

    public static Unit box() {
	return uvalue;
    }

    public static Boolean box(boolean x) {
        return x ? zvalue_t : zvalue_f;
    }

    public static Byte box(byte x) {
        return bvalue[x & 0x000000FF];
    }

    public static Short box(short x) {
        int c = x & CHECK_MASK;
        if (c == 0 || c == CHECK_MASK) return svalue[x & INDEX_MASK];
        return new SValue(x);
    }

    public static Char box(char x) {
        int c = (int)x & CHECK_MASK;
        if (c == 0) return cvalue[(int)x & INDEX_MASK];
        return new CValue(x);
    }

    public static Int box(int x) {
        int c = x & CHECK_MASK;
        if (c == 0 || c == CHECK_MASK) return ivalue[x & INDEX_MASK];
        return new IValue(x);
    }

    public static Long box(long x) {
        long c = x & CHECK_MASK;
        if (c == 0 || c == CHECK_MASK) return lvalue[(int)x & INDEX_MASK];
        return new LValue(x);
    }

    public static Float box(float x) {
        return new FValue(x);
    }

    public static Double box(double x) {
        return new DValue(x);
    }

    /** @meta method (scala.Array[scala.Boolean]) scala.Array[scala.Boolean];*/
    public static Array box(boolean[] xs) {
	return new ZArray(xs);
    }

    /** @meta method (scala.Array[scala.Byte]) scala.Array[scala.Byte]; */
    public static Array box(byte[] xs) {
	return new BArray(xs);
    }

    /** @meta method (scala.Array[scala.Short]) scala.Array[scala.Short]; */
    public static Array box(short[] xs) {
	return new SArray(xs);
    }

    /** @meta method (scala.Array[scala.Char]) scala.Array[scala.Char]; */
    public static Array box(char[] xs) {
	return new CArray(xs);
    }

    /** @meta method (scala.Array[scala.Int]) scala.Array[scala.Int]; */
    public static Array box(int[] xs) {
	return new IArray(xs);
    }

    /** @meta method (scala.Array[scala.Long]) scala.Array[scala.Long]; */
    public static Array box(long[] xs) {
	return new LArray(xs);
    }

    /** @meta method (scala.Array[scala.Float]) scala.Array[scala.Float]; */
    public static Array box(float[] xs) {
	return new FArray(xs);
    }

    /** @meta method (scala.Array[scala.Double]) scala.Array[scala.Double]; */
    public static Array box(double[] xs) {
	return new DArray(xs);
    }

    /** @meta method [?T < scala.AnyRef](scala.Array[?T]) scala.Array[?T]; */
    public static Array box(Object[] xs) {
	return new OArray(xs);
    }

    /** @meta method [?T](scala.Array[?T]) scala.Array[?T]; */
    public static Array box(Object xs) {
        if (xs == null) return box((Object[])xs);
        if (xs instanceof Object[]) return box((Object[])xs);
        if (xs instanceof boolean[]) return box((boolean[])xs);
        if (xs instanceof byte[]) return box((byte[])xs);
        if (xs instanceof short[]) return box((short[])xs);
        if (xs instanceof char[]) return box((char[])xs);
        if (xs instanceof int[]) return box((int[])xs);
        if (xs instanceof long[]) return box((long[])xs);
        if (xs instanceof float[]) return box((float[])xs);
        if (xs instanceof double[]) return box((double[])xs);
        throw new ClassCastException(xs.getClass() + " is not an array class");
    }

    //########################################################################
    // Public Functions - Unboxing primitives

    public static void      unbox_uvalue(Unit    x) {        x.asUnit   (); }
    public static boolean   unbox_zvalue(Boolean x) { return x.asBoolean(); }
    public static byte      unbox_bvalue(Byte    x) { return x.asByte   (); }
    public static short     unbox_svalue(Short   x) { return x.asShort  (); }
    public static char      unbox_cvalue(Char    x) { return x.asChar   (); }
    public static int       unbox_ivalue(Int     x) { return x.asInt    (); }
    public static long      unbox_lvalue(Long    x) { return x.asLong   (); }
    public static float     unbox_fvalue(Float   x) { return x.asFloat  (); }
    public static double    unbox_dvalue(Double  x) { return x.asDouble (); }

    /** @meta method (scala.Array[scala.Boolean]) scala.Array[scala.Boolean];*/
    public static boolean[] unbox_zarray(Array xs) {
        return xs == null ? null : xs.asBooleanArray();
    }
    /** @meta method (scala.Array[scala.Byte]) scala.Array[scala.Byte]; */
    public static byte   [] unbox_barray(Array xs) {
        return xs == null ? null : xs.asByteArray   ();
    }
    /** @meta method (scala.Array[scala.Short]) scala.Array[scala.Short]; */
    public static short  [] unbox_sarray(Array xs) {
        return xs == null ? null : xs.asShortArray  ();
    }
    /** @meta method (scala.Array[scala.Char]) scala.Array[scala.Char]; */
    public static char   [] unbox_carray(Array xs) {
        return xs == null ? null : xs.asCharArray   ();
    }
    /** @meta method (scala.Array[scala.Int]) scala.Array[scala.Int]; */
    public static int    [] unbox_iarray(Array xs) {
        return xs == null ? null : xs.asIntArray    ();
    }
    /** @meta method (scala.Array[scala.Long]) scala.Array[scala.Long]; */
    public static long   [] unbox_larray(Array xs) {
        return xs == null ? null : xs.asLongArray   ();
    }
    /** @meta method (scala.Array[scala.Float]) scala.Array[scala.Float]; */
    public static float  [] unbox_farray(Array xs) {
        return xs == null ? null : xs.asFloatArray  ();
    }
    /** @meta method (scala.Array[scala.Double]) scala.Array[scala.Double]; */
    public static double [] unbox_darray(Array xs) {
        return xs == null ? null : xs.asDoubleArray ();
    }
    /** @meta method [?T < scala.AnyRef](scala.Array[?T]) scala.Array[?T]; */
    public static Object [] unbox_oarray(Array xs) {
        return xs == null ? null : xs.asObjectArray ();
    }
    /** @meta method [?T](scala.Array[?T]) scala.Array[?T]; */
    public static Object    unbox__array(Array xs) {
        return xs == null ? null : xs.asArray       ();
    }

    //########################################################################
    // Public Functions - Conversion primitives

    public static byte   b2b(byte   x) { return (byte  )x; }
    public static short  b2s(byte   x) { return (short )x; }
    public static char   b2c(byte   x) { return (char  )x; }
    public static int    b2i(byte   x) { return (int   )x; }
    public static long   b2l(byte   x) { return (long  )x; }
    public static float  b2f(byte   x) { return (float )x; }
    public static double b2d(byte   x) { return (double)x; }
    public static byte   s2b(short  x) { return (byte  )x; }
    public static short  s2s(short  x) { return (short )x; }
    public static char   s2c(short  x) { return (char  )x; }
    public static int    s2i(short  x) { return (int   )x; }
    public static long   s2l(short  x) { return (long  )x; }
    public static float  s2f(short  x) { return (float )x; }
    public static double s2d(short  x) { return (double)x; }
    public static byte   c2b(char   x) { return (byte  )x; }
    public static short  c2s(char   x) { return (short )x; }
    public static char   c2c(char   x) { return (char  )x; }
    public static int    c2i(char   x) { return (int   )x; }
    public static long   c2l(char   x) { return (long  )x; }
    public static float  c2f(char   x) { return (float )x; }
    public static double c2d(char   x) { return (double)x; }
    public static byte   i2b(int    x) { return (byte  )x; }
    public static short  i2s(int    x) { return (short )x; }
    public static char   i2c(int    x) { return (char  )x; }
    public static int    i2i(int    x) { return (int   )x; }
    public static long   i2l(int    x) { return (long  )x; }
    public static float  i2f(int    x) { return (float )x; }
    public static double i2d(int    x) { return (double)x; }
    public static byte   l2b(long   x) { return (byte  )x; }
    public static short  l2s(long   x) { return (short )x; }
    public static char   l2c(long   x) { return (char  )x; }
    public static int    l2i(long   x) { return (int   )x; }
    public static long   l2l(long   x) { return (long  )x; }
    public static float  l2f(long   x) { return (float )x; }
    public static double l2d(long   x) { return (double)x; }
    public static byte   f2b(float  x) { return (byte  )x; }
    public static short  f2s(float  x) { return (short )x; }
    public static char   f2c(float  x) { return (char  )x; }
    public static int    f2i(float  x) { return (int   )x; }
    public static long   f2l(float  x) { return (long  )x; }
    public static float  f2f(float  x) { return (float )x; }
    public static double f2d(float  x) { return (double)x; }
    public static byte   d2b(double x) { return (byte  )x; }
    public static short  d2s(double x) { return (short )x; }
    public static char   d2c(double x) { return (char  )x; }
    public static int    d2i(double x) { return (int   )x; }
    public static long   d2l(double x) { return (long  )x; }
    public static float  d2f(double x) { return (float )x; }
    public static double d2d(double x) { return (double)x; }

    //########################################################################
    // Public Functions - Array primitives

    public static boolean[] zarray(int length) { return new boolean[length]; }
    public static byte   [] barray(int length) { return new byte   [length]; }
    public static short  [] sarray(int length) { return new short  [length]; }
    public static char   [] carray(int length) { return new char   [length]; }
    public static int    [] iarray(int length) { return new int    [length]; }
    public static long   [] larray(int length) { return new long   [length]; }
    public static float  [] farray(int length) { return new float  [length]; }
    public static double [] darray(int length) { return new double [length]; }
    public static Object    oarray(int length, String classname) {
        try {
            Class clasz = Class.forName(classname, false, loader);
            return java.lang.reflect.Array.newInstance(clasz, length);
        } catch (ClassNotFoundException exception) {
            throw new Error(exception.toString());
        }
    }

    public static int zarray_length(boolean[] xs) { return xs.length; }
    public static int barray_length(byte   [] xs) { return xs.length; }
    public static int sarray_length(short  [] xs) { return xs.length; }
    public static int carray_length(char   [] xs) { return xs.length; }
    public static int iarray_length(int    [] xs) { return xs.length; }
    public static int larray_length(long   [] xs) { return xs.length; }
    public static int farray_length(float  [] xs) { return xs.length; }
    public static int darray_length(double [] xs) { return xs.length; }
    public static int oarray_length(Object [] xs) { return xs.length; }

    public static boolean zarray_get(boolean[] xs, int i) { return xs[i]; }
    public static byte    barray_get(byte   [] xs, int i) { return xs[i]; }
    public static short   sarray_get(short  [] xs, int i) { return xs[i]; }
    public static char    carray_get(char   [] xs, int i) { return xs[i]; }
    public static int     iarray_get(int    [] xs, int i) { return xs[i]; }
    public static long    larray_get(long   [] xs, int i) { return xs[i]; }
    public static float   farray_get(float  [] xs, int i) { return xs[i]; }
    public static double  darray_get(double [] xs, int i) { return xs[i]; }
    public static Object  oarray_get(Object [] xs, int i) { return xs[i]; }

    public static void zarray_set(boolean[] xs, int i, boolean x) { xs[i] = x;}
    public static void barray_set(byte   [] xs, int i, byte    x) { xs[i] = x;}
    public static void sarray_set(short  [] xs, int i, short   x) { xs[i] = x;}
    public static void carray_set(char   [] xs, int i, char    x) { xs[i] = x;}
    public static void iarray_set(int    [] xs, int i, int     x) { xs[i] = x;}
    public static void larray_set(long   [] xs, int i, long    x) { xs[i] = x;}
    public static void farray_set(float  [] xs, int i, float   x) { xs[i] = x;}
    public static void darray_set(double [] xs, int i, double  x) { xs[i] = x;}
    public static void oarray_set(Object [] xs, int i, Object  x) { xs[i] = x;}

    //########################################################################
}

// These classes may not be defined in class RunTime because inner
// classes confuse pico which then attributes the metadata to the
// wrong members.

class UValue extends Unit {
}

class ZValue extends Boolean {
    private final boolean x;
    public ZValue(boolean x) { this.x = x; }
    public boolean asBoolean() { return x; }
}

class BValue extends Byte {
    private final byte x;
    public BValue(byte x) { this.x = x; }
    public double asDouble() { return (double)x; }
    public float asFloat() { return (float)x; }
    public long asLong() { return (long)x; }
    public int asInt() { return (int)x; }
    public char asChar() { return (char)x; }
    public short asShort() { return (short)x; }
    public byte asByte() { return (byte)x; }
}

class SValue extends Short {
    private final short x;
    public SValue(short x) { this.x = x; }
    public double asDouble() { return (double)x; }
    public float asFloat() { return (float)x; }
    public long asLong() { return (long)x; }
    public int asInt() { return (int)x; }
    public char asChar() { return (char)x; }
    public short asShort() { return (short)x; }
    public byte asByte() { return (byte)x; }
}

class CValue extends Char {
    private final char x;
    public CValue(char x) { this.x = x; }
    public double asDouble() { return (double)x; }
    public float asFloat() { return (float)x; }
    public long asLong() { return (long)x; }
    public int asInt() { return (int)x; }
    public char asChar() { return (char)x; }
    public short asShort() { return (short)x; }
    public byte asByte() { return (byte)x; }
}

class IValue extends Int {
    private final int x;
    public IValue(int x) { this.x = x; }
    public double asDouble() { return (double)x; }
    public float asFloat() { return (float)x; }
    public long asLong() { return (long)x; }
    public int asInt() { return (int)x; }
    public char asChar() { return (char)x; }
    public short asShort() { return (short)x; }
    public byte asByte() { return (byte)x; }
}

class LValue extends Long {
    private final long x;
    public LValue(long x) { this.x = x; }
    public double asDouble() { return (double)x; }
    public float asFloat() { return (float)x; }
    public long asLong() { return (long)x; }
    public int asInt() { return (int)x; }
    public char asChar() { return (char)x; }
    public short asShort() { return (short)x; }
    public byte asByte() { return (byte)x; }
}

class FValue extends Float {
    private final float x;
    public FValue(float x) { this.x = x; }
    public double asDouble() { return (double)x; }
    public float asFloat() { return (float)x; }
    public long asLong() { return (long)x; }
    public int asInt() { return (int)x; }
    public char asChar() { return (char)x; }
    public short asShort() { return (short)x; }
    public byte asByte() { return (byte)x; }
}

class DValue extends Double {
    private final double x;
    public DValue(double x) { this.x = x; }
    public double asDouble() { return (double)x; }
    public float asFloat() { return (float)x; }
    public long asLong() { return (long)x; }
    public int asInt() { return (int)x; }
    public char asChar() { return (char)x; }
    public short asShort() { return (short)x; }
    public byte asByte() { return (byte)x; }
}

class ZArray extends Array {
    private final boolean[] xs;
    public ZArray(boolean[] xs) { this.xs = xs; }
    public boolean[] asBooleanArray() { return xs; }
    public Object asArray() { return xs; }
    public Object apply(int i) { return RunTime.box(xs[i]); }
    public void update(int i, Object x) { xs[i] = ((Boolean)x).asBoolean(); }
    public int length() { return xs.length; }
    public String toString() { return String.valueOf(xs); }
}

class BArray extends Array {
    private final byte[] xs;
    public BArray(byte[] xs) { this.xs = xs; }
    public byte[] asByteArray() { return xs; }
    public Object asArray() { return xs; }
    public Object apply(int i) { return RunTime.box(xs[i]); }
    public void update(int i, Object x) { xs[i] = ((Byte)x).asByte(); }
    public int length() { return xs.length; }
    public String toString() { return String.valueOf(xs); }
}

class SArray extends Array {
    private final short[] xs;
    public SArray(short[] xs) { this.xs = xs; }
    public short[] asShortArray() { return xs; }
    public Object asArray() { return xs; }
    public Object apply(int i) { return RunTime.box(xs[i]); }
    public void update(int i, Object x) { xs[i] = ((Short)x).asShort(); }
    public int length() { return xs.length; }
    public String toString() { return String.valueOf(xs); }
}

class CArray extends Array {
    private final char[] xs;
    public CArray(char[] xs) { this.xs = xs; }
    public char[] asCharArray() { return xs; }
    public Object asArray() { return xs; }
    public Object apply(int i) { return RunTime.box(xs[i]); }
    public void update(int i, Object x) { xs[i] = ((Char)x).asChar(); }
    public int length() { return xs.length; }
    public String toString() { return String.valueOf((Object)xs); }
}

class IArray extends Array {
    private final int[] xs;
    public IArray(int[] xs) { this.xs = xs; }
    public int[] asIntArray() { return xs; }
    public Object asArray() { return xs; }
    public Object apply(int i) { return RunTime.box(xs[i]); }
    public void update(int i, Object x) { xs[i] = ((Int)x).asInt(); }
    public int length() { return xs.length; }
    public String toString() { return String.valueOf(xs); }
}

class LArray extends Array {
    private final long[] xs;
    public LArray(long[] xs) { this.xs = xs; }
    public long[] asLongArray() { return xs; }
    public Object asArray() { return xs; }
    public Object apply(int i) { return RunTime.box(xs[i]); }
    public void update(int i, Object x) { xs[i] = ((Long)x).asLong(); }
    public int length() { return xs.length; }
    public String toString() { return String.valueOf(xs); }
}

class FArray extends Array {
    private final float[] xs;
    public FArray(float[] xs) { this.xs = xs; }
    public float[] asFloatArray() { return xs; }
    public Object asArray() { return xs; }
    public Object apply(int i) { return RunTime.box(xs[i]); }
    public void update(int i, Object x) { xs[i] = ((Float)x).asFloat(); }
    public int length() { return xs.length; }
    public String toString() { return String.valueOf(xs); }
}

class DArray extends Array {
    private final double[] xs;
    public DArray(double[] xs) { this.xs = xs; }
    public double[] asDoubleArray() { return xs; }
    public Object asArray() { return xs; }
    public Object apply(int i) { return RunTime.box(xs[i]); }
    public void update(int i, Object x) { xs[i] = ((Double)x).asDouble(); }
    public int length() { return xs.length; }
    public String toString() { return String.valueOf(xs); }
}

class OArray extends Array {
    private final Object[] xs;
    public OArray(Object[] xs) { this.xs = xs; }
    public Object[] asObjectArray() { return xs; }
    public Object asArray() { return xs; }
    public Object apply(int i) { return xs[i]; }
    public void update(int i, Object x) { xs[i] = x; }
    public int length() { return xs.length; }
    public String toString() { return String.valueOf(xs); }
}
