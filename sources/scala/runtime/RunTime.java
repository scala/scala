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

    private static final int BITS        = 8;
    private static final int ARRAY_SIZE  = 2 << BITS;
    private static final int INDEX_MASK  = ARRAY_SIZE - 1;
    private static final int CHECK_MASK  = ~(ARRAY_SIZE / 2 - 1);

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
    // Public Functions - Catching exceptions

    public static Throwable tryCatch(Runnable runnable) {
	try {
	    runnable.run();
            return null;
	} catch (Throwable exception) {
	    return exception;
	}
    }

    //########################################################################
    // Public Functions - Boxing primitives

    public static Unit    box_uvalue(         ) {
        return uvalue;
    }

    public static Boolean box_zvalue(boolean x) {
        return x ? zvalue_t : zvalue_f;
    }

    public static Byte    box_bvalue(byte    x) {
        return bvalue[x & 0x000000FF];
    }

    public static Short   box_svalue(short   x) {
        int c = x & CHECK_MASK;
        if (c == 0 || c == CHECK_MASK) return svalue[x & INDEX_MASK];
        return new SValue(x);
    }

    public static Char    box_cvalue(char    x) {
        int c = (int)x & CHECK_MASK;
        if (c == 0) return cvalue[(int)x & INDEX_MASK];
        return new CValue(x);
    }

    public static Int     box_ivalue(int     x) {
        int c = x & CHECK_MASK;
        if (c == 0 || c == CHECK_MASK) return ivalue[x & INDEX_MASK];
        return new IValue(x);
    }

    public static Long    box_lvalue(long    x) {
        long c = x & CHECK_MASK;
        if (c == 0 || c == CHECK_MASK) return lvalue[(int)x & INDEX_MASK];
        return new LValue(x);
    }

    public static Float   box_fvalue(float   x) {
        return new FValue(x);
    }

    public static Double  box_dvalue(double  x) {
        return new DValue(x);
    }

    /** @meta method (scala.Array[scala.Boolean]) scala.Array[scala.Boolean];*/
    public static Array   box_zarray(boolean[] xs) {
        return new ZArray(xs);
    }

    /** @meta method (scala.Array[scala.Byte]) scala.Array[scala.Byte]; */
    public static Array   box_barray(byte   [] xs) {
        return new BArray(xs);
    }

    /** @meta method (scala.Array[scala.Short]) scala.Array[scala.Short]; */
    public static Array   box_sarray(short  [] xs) {
        return new SArray(xs);
    }

    /** @meta method (scala.Array[scala.Char]) scala.Array[scala.Char]; */
    public static Array   box_carray(char   [] xs) {
        return new CArray(xs);
    }

    /** @meta method (scala.Array[scala.Int]) scala.Array[scala.Int]; */
    public static Array   box_iarray(int    [] xs) {
        return new IArray(xs);
    }

    /** @meta method (scala.Array[scala.Long]) scala.Array[scala.Long]; */
    public static Array   box_larray(long   [] xs) {
        return new LArray(xs);
    }

    /** @meta method (scala.Array[scala.Float]) scala.Array[scala.Float]; */
    public static Array   box_farray(float  [] xs) {
        return new FArray(xs);
    }

    /** @meta method (scala.Array[scala.Double]) scala.Array[scala.Double]; */
    public static Array   box_darray(double [] xs) {
        return new DArray(xs);
    }

    /** @meta method [?T < scala.AnyRef](scala.Array[?T]) scala.Array[?T]; */
    public static Array   box_oarray(Object [] xs) {
        return new OArray(xs);
    }

    /** @meta method [?T](scala.Array[?T]) scala.Array[?T]; */
    public static Array   box__array(Object    xs) {
        if (xs == null             ) return box_oarray((Object [])xs);
        if (xs instanceof boolean[]) return box_zarray((boolean[])xs);
        if (xs instanceof byte   []) return box_barray((byte   [])xs);
        if (xs instanceof short  []) return box_sarray((short  [])xs);
        if (xs instanceof char   []) return box_carray((char   [])xs);
        if (xs instanceof int    []) return box_iarray((int    [])xs);
        if (xs instanceof long   []) return box_larray((long   [])xs);
        if (xs instanceof float  []) return box_farray((float  [])xs);
        if (xs instanceof double []) return box_darray((double [])xs);
        if (xs instanceof Object []) return box_oarray((Object [])xs);
        throw new ClassCastException(xs.getClass() + " is not an array class");
    }

    //########################################################################
    // Public Functions - Unboxing primitives

    public static void      unbox_uvalue(Unit    x) {        x.value(); }
    public static boolean   unbox_zvalue(Boolean x) { return x.value  ; }
    public static byte      unbox_bvalue(Byte    x) { return x.value  ; }
    public static short     unbox_svalue(Short   x) { return x.value  ; }
    public static char      unbox_cvalue(Char    x) { return x.value  ; }
    public static int       unbox_ivalue(Int     x) { return x.value  ; }
    public static long      unbox_lvalue(Long    x) { return x.value  ; }
    public static float     unbox_fvalue(Float   x) { return x.value  ; }
    public static double    unbox_dvalue(Double  x) { return x.value  ; }

    /** @meta method (scala.Array[scala.Boolean]) scala.Array[scala.Boolean];*/
    public static boolean[] unbox_zarray(Array xs) {
        return xs == null ? null : ((ZArray)xs).value;
    }
    /** @meta method (scala.Array[scala.Byte]) scala.Array[scala.Byte]; */
    public static byte   [] unbox_barray(Array xs) {
        return xs == null ? null : ((BArray)xs).value;
    }
    /** @meta method (scala.Array[scala.Short]) scala.Array[scala.Short]; */
    public static short  [] unbox_sarray(Array xs) {
        return xs == null ? null : ((SArray)xs).value;
    }
    /** @meta method (scala.Array[scala.Char]) scala.Array[scala.Char]; */
    public static char   [] unbox_carray(Array xs) {
        return xs == null ? null : ((CArray)xs).value;
    }
    /** @meta method (scala.Array[scala.Int]) scala.Array[scala.Int]; */
    public static int    [] unbox_iarray(Array xs) {
        return xs == null ? null : ((IArray)xs).value;
    }
    /** @meta method (scala.Array[scala.Long]) scala.Array[scala.Long]; */
    public static long   [] unbox_larray(Array xs) {
        return xs == null ? null : ((LArray)xs).value;
    }
    /** @meta method (scala.Array[scala.Float]) scala.Array[scala.Float]; */
    public static float  [] unbox_farray(Array xs) {
        return xs == null ? null : ((FArray)xs).value;
    }
    /** @meta method (scala.Array[scala.Double]) scala.Array[scala.Double]; */
    public static double [] unbox_darray(Array xs) {
        return xs == null ? null : ((DArray)xs).value;
    }
    /** @meta method [?T < scala.AnyRef](scala.Array[?T]) scala.Array[scala.AnyRef]; */
    public static Object [] unbox_oarray(Array xs) {
        return xs == null ? null : ((OArray)xs).value;
    }
    /** @meta method [?T](scala.Array[?T]) scala.AnyRef; */
    public static Object    unbox__array(Array xs) {
        return xs == null ? null : xs.value();
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

final class UValue extends Unit    { public UValue(         ) { super( ); } }
final class ZValue extends Boolean { public ZValue(boolean x) { super(x); } }
final class BValue extends Byte    { public BValue(byte    x) { super(x); } }
final class SValue extends Short   { public SValue(short   x) { super(x); } }
final class CValue extends Char    { public CValue(char    x) { super(x); } }
final class IValue extends Int     { public IValue(int     x) { super(x); } }
final class LValue extends Long    { public LValue(long    x) { super(x); } }
final class FValue extends Float   { public FValue(float   x) { super(x); } }
final class DValue extends Double  { public DValue(double  x) { super(x); } }

/** @meta class extends scala.Array[scala.Boolean]; */
final class ZArray extends Array {
    public final boolean[] value;
    public ZArray(boolean[] value) { this.value = value; }
    public Object value() { return value; }
    public Object apply(int i) { return RunTime.box_zvalue(value[i]); }
    public void update(int i, Object x) { value[i] = ((Boolean)x).value; }
    public int length() { return value.length; }
    public String toString() { return String.valueOf(value); }
}

/** @meta class extends scala.Array[scala.Byte]; */
final class BArray extends Array {
    public final byte[] value;
    public BArray(byte[] value) { this.value = value; }
    public Object value() { return value; }
    public Object apply(int i) { return RunTime.box_bvalue(value[i]); }
    public void update(int i, Object x) { value[i] = ((Byte)x).value; }
    public int length() { return value.length; }
    public String toString() { return String.valueOf(value); }
}

/** @meta class extends scala.Array[scala.Short]; */
final class SArray extends Array {
    public final short[] value;
    public SArray(short[] value) { this.value = value; }
    public Object value() { return value; }
    public Object apply(int i) { return RunTime.box_svalue(value[i]); }
    public void update(int i, Object x) { value[i] = ((Short)x).value; }
    public int length() { return value.length; }
    public String toString() { return String.valueOf(value); }
}

/** @meta class extends scala.Array[scala.Char]; */
final class CArray extends Array {
    public final char[] value;
    public CArray(char[] value) { this.value = value; }
    public Object value() { return value; }
    public Object apply(int i) { return RunTime.box_cvalue(value[i]); }
    public void update(int i, Object x) { value[i] = ((Char)x).value; }
    public int length() { return value.length; }
    public String toString() { return String.valueOf((Object)value); }
}

/** @meta class extends scala.Array[scala.Int]; */
final class IArray extends Array {
    public final int[] value;
    public IArray(int[] value) { this.value = value; }
    public Object value() { return value; }
    public Object apply(int i) { return RunTime.box_ivalue(value[i]); }
    public void update(int i, Object x) { value[i] = ((Int)x).value; }
    public int length() { return value.length; }
    public String toString() { return String.valueOf(value); }
}

/** @meta class extends scala.Array[scala.Long]; */
final class LArray extends Array {
    public final long[] value;
    public LArray(long[] value) { this.value = value; }
    public Object value() { return value; }
    public Object apply(int i) { return RunTime.box_lvalue(value[i]); }
    public void update(int i, Object x) { value[i] = ((Long)x).value; }
    public int length() { return value.length; }
    public String toString() { return String.valueOf(value); }
}

/** @meta class extends scala.Array[scala.Float]; */
final class FArray extends Array {
    public final float[] value;
    public FArray(float[] value) { this.value = value; }
    public Object value() { return value; }
    public Object apply(int i) { return RunTime.box_fvalue(value[i]); }
    public void update(int i, Object x) { value[i] = ((Float)x).value; }
    public int length() { return value.length; }
    public String toString() { return String.valueOf(value); }
}

/** @meta class extends scala.Array[scala.Double]; */
final class DArray extends Array {
    public final double[] value;
    public DArray(double[] value) { this.value = value; }
    public Object value() { return value; }
    public Object apply(int i) { return RunTime.box_dvalue(value[i]); }
    public void update(int i, Object x) { value[i] = ((Double)x).value; }
    public int length() { return value.length; }
    public String toString() { return String.valueOf(value); }
}

/** @meta class [?T < scala.AnyRef] extends scala.Array[?T]; */
final class OArray extends Array {
    public final Object[] value;
    public OArray(Object[] value) { this.value = value; }
    public Object value() { return value; }
    public Object apply(int i) { return value[i]; }
    public void update(int i, Object x) { value[i] = x; }
    public int length() { return value.length; }
    public String toString() { return String.valueOf(value); }
}
