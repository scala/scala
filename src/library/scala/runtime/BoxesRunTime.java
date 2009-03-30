/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.runtime;

/** An object (static class) that defines methods used for creating,
  * reverting, and calculating with, boxed values. There are four classes
  * of methods in this object:
  *   - High-performance value boxing methods that feed from a pre-
  *     computed map of instances for the most common instanciations.
  *   - Convenience unboxing methods returning default value on null.
  *   - The generalised comparison method to be used when an object may
  *     be a boxed value.
  *   - Standard value operators for boxed number and quasi-number values.
  *
  * @author  Gilles Dubochet
  * @author  Martin Odersky
  * @contributor Stepan Koltsov
  * @version 2.0 */
public class BoxesRunTime {

    private static final int CHAR = 0, BYTE = 1, SHORT = 2, INT = 3, LONG = 4, FLOAT = 5, DOUBLE = 6, OTHER = 7;

    private static int typeCode(Object a) {
        if (a instanceof Integer) return INT;
        if (a instanceof Character) return CHAR;
        if (a instanceof Long) return LONG;
        if (a instanceof Double) return DOUBLE;
        if (a instanceof Float) return FLOAT;
        if (a instanceof Byte) return BYTE;
        if (a instanceof Short) return SHORT;
        return OTHER;
    }

/* BOXING ... BOXING ... BOXING ... BOXING ... BOXING ... BOXING ... BOXING ... BOXING */

    // for 2.8: change to valueOf methods.

    private static int charLowBound = 0;
    private static int charUpBound = 255;
    private static Character[] charCache = new Character[charUpBound - charLowBound + 1];

    private static int byteLowBound = -128;
    private static int byteUpBound = 127;
    private static Byte[] byteCache = new Byte[byteUpBound - byteLowBound + 1];

    private static int shortLowBound = -128;
    private static int shortUpBound = 127;
    private static Short[] shortCache = new Short[shortUpBound - shortLowBound + 1];

    private static int intLowBound = -128;
    private static int intUpBound = 1024;
    private static Integer[] intCache = new Integer[intUpBound - intLowBound + 1];

    private static int longLowBound = -128;
    private static int longUpBound = 1024;
    private static Long[] longCache = new Long[longUpBound - longLowBound + 1];

    static {
        int idx = 0;
        while (idx <= charUpBound - charLowBound) {
            charCache[idx] = new Character((char)(idx + charLowBound));
            idx = idx + 1;
        }
        idx = 0;
        while (idx <= byteUpBound - byteLowBound) {
            byteCache[idx] = new Byte((byte)(idx + byteLowBound));
            idx = idx + 1;
        }
        idx = 0;
        while (idx <= shortUpBound - shortLowBound) {
            shortCache[idx] = new Short((short)(idx + shortLowBound));
            idx = idx + 1;
        }
        idx = 0;
        while (idx <= intUpBound - intLowBound) {
            intCache[idx] = new Integer((int)(idx + intLowBound));
            idx = idx + 1;
        }
        idx = 0;
        while (idx <= longUpBound - longLowBound) {
            longCache[idx] = new Long((long)(idx + longLowBound));
            idx = idx + 1;
        }
    }

    public static Boolean boxToBoolean(boolean b) {
        return b ? Boolean.TRUE : Boolean.FALSE;
    }

    public static Character boxToCharacter(char c) {
        if (c >= charLowBound && c <= charUpBound)
            return charCache[(int)c - charLowBound];
        return new Character(c);
    }

    public static Byte boxToByte(byte b) {
        if (b >= byteLowBound && b <= byteUpBound)
            return byteCache[(int)b - byteLowBound];
        return new Byte(b);
    }

    public static Short boxToShort(short s) {
        if (s >= shortLowBound && s <= shortUpBound)
            return shortCache[(int)s - shortLowBound];
        return new Short(s);
    }

    public static Integer boxToInteger(int i) {
        if (i >= intLowBound && i <= intUpBound)
            return intCache[(int)i - intLowBound];
        return new Integer(i);
    }

    public static Long boxToLong(long l) {
        if (l >= longLowBound && l <= longUpBound)
            return longCache[(int)l - longLowBound];
        return new Long(l);
    }

    public static Float boxToFloat(float f) {
        return new Float(f);
    }

    public static Double boxToDouble(double d) {
        return new Double(d);
    }

/* UNBOXING ... UNBOXING ... UNBOXING ... UNBOXING ... UNBOXING ... UNBOXING ... UNBOXING */

    public static boolean unboxToBoolean(Object b) {
        return b == null ? false : ((Boolean)b).booleanValue();
    }

    public static char unboxToChar(Object c) {
        return c == null ? 0 : ((Character)c).charValue();
    }

    public static byte unboxToByte(Object b) {
        return b == null ? 0 : ((Byte)b).byteValue();
    }

    public static short unboxToShort(Object s) {
        return s == null ? 0 : ((Short)s).shortValue();
    }

    public static int unboxToInt(Object i) {
        return i == null ? 0 : ((Integer)i).intValue();
    }

    public static long unboxToLong(Object l) {
        return l == null ? 0 : ((Long)l).longValue();
    }

    public static float unboxToFloat(Object f) {
        return f == null ? 0.0f : ((Float)f).floatValue();
    }

    public static double unboxToDouble(Object d) {
        return d == null ? 0.0d : ((Double)d).doubleValue();
    }

    /*
    public static boolean unboxToBoolean(Object b) {
        if (b == null)
          throw new ClassCastException("null is no Boolean value");
        return ((Boolean)b).booleanValue();
    }

    public static char unboxToChar(Object c) {
        if (c == null)
          throw new ClassCastException("null is no Char value");
        return ((Character)c).charValue();
    }

    public static byte unboxToByte(Object b) {
        if (b == null)
          throw new ClassCastException("null is no Byte value");
        return ((Byte)b).byteValue();
    }

    public static short unboxToShort(Object s) {
        if (s == null)
          throw new ClassCastException("null is no Short value");
        return ((Short)s).shortValue();
    }

    public static int unboxToInt(Object i) {
        if (i == null)
          throw new ClassCastException("null is no Int value");
        return ((Integer)i).intValue();
    }

    public static long unboxToLong(Object l) {
        if (l == null)
          throw new ClassCastException("null is no Long value");
        return ((Long)l).longValue();
    }

    public static float unboxToFloat(Object f) {
        if (f == null)
          throw new ClassCastException("null is no Float value");
        return ((Float)f).floatValue();
    }

    public static double unboxToDouble(Object d) {
        if (d == null)
          throw new ClassCastException("null is no Double value");
        return ((Double)d).doubleValue();
    }
    */

/* COMPARISON ... COMPARISON ... COMPARISON ... COMPARISON ... COMPARISON ... COMPARISON */

    /** A rich implementation of the <code>equals</code> method that overrides the
      * default equals because Java's boxed primitives are utterly broken. This equals
      * is inserted instead of a normal equals by the Scala compiler (in the
      * ICode phase, method <code>genEqEqPrimitive</code>) only when either
      * side of the comparison is a subclass of <code>AnyVal</code>, of
      * <code>java.lang.Number</code>, of <code>java.lang.Character</code> or
      * is exactly <code>Any</code> or <code>AnyRef</code>. */
    public static boolean equals(Object a, Object b) {
        if (a == null || b == null)
            return a == b;
        if (a.equals(b))
            return true;
        if (a instanceof Number || a instanceof Character || b instanceof Number || b instanceof Character) {
            int acode = typeCode(a);
            int bcode = typeCode(b);
            int maxcode = (acode < bcode) ? bcode : acode;
            if (maxcode <= INT) {
                int aa = (acode == CHAR) ? ((Character) a).charValue() : ((Number) a).intValue();
                int bb = (bcode == CHAR) ? ((Character) b).charValue() : ((Number) b).intValue();
                return aa == bb;
            }
            if (maxcode <= LONG) {
                long aa = (acode == CHAR) ? ((Character) a).charValue() : ((Number) a).longValue();
                long bb = (bcode == CHAR) ? ((Character) b).charValue() : ((Number) b).longValue();
                return aa == bb;
            }
            if (maxcode <= FLOAT) {
                float aa = (acode == CHAR) ? ((Character) a).charValue() : ((Number) a).floatValue();
                float bb = (bcode == CHAR) ? ((Character) b).charValue() : ((Number) b).floatValue();
                return aa == bb;
            }
            if (maxcode <= DOUBLE) {
                double aa = (acode == CHAR) ? ((Character) a).charValue() : ((Number) a).doubleValue();
                double bb = (bcode == CHAR) ? ((Character) b).charValue() : ((Number) b).doubleValue();
                return aa == bb;
            }
			return b.equals(a);
        }
        return false;
    }

/* OPERATORS ... OPERATORS ... OPERATORS ... OPERATORS ... OPERATORS ... OPERATORS ... OPERATORS ... OPERATORS */

    /** arg1 + arg2 */
    public static Object add(Object arg1, Object arg2) throws NoSuchMethodException {
        int code1 = typeCode(arg1);
        int code2 = typeCode(arg2);
        int maxcode = (code1 < code2) ? code2 : code1;
        if (maxcode <= INT) {
            int val1 = (code1 == CHAR) ? ((Character) arg1).charValue() : ((Number) arg1).intValue();
            int val2 = (code2 == CHAR) ? ((Character) arg2).charValue() : ((Number) arg2).intValue();
            return boxToInteger(val1 + val2);
        }
        if (maxcode <= LONG) {
            long val1 = (code1 == CHAR) ? ((Character) arg1).charValue() : ((Number) arg1).longValue();
            long val2 = (code2 == CHAR) ? ((Character) arg2).charValue() : ((Number) arg2).longValue();
            return boxToLong(val1 + val2);
        }
        if (maxcode <= FLOAT) {
            float val1 = (code1 == CHAR) ? ((Character) arg1).charValue() : ((Number) arg1).floatValue();
            float val2 = (code2 == CHAR) ? ((Character) arg2).charValue() : ((Number) arg2).floatValue();
            return boxToFloat(val1 + val2);
        }
        if (maxcode <= DOUBLE) {
            double val1 = (code1 == CHAR) ? ((Character) arg1).charValue() : ((Number) arg1).doubleValue();
            double val2 = (code2 == CHAR) ? ((Character) arg2).charValue() : ((Number) arg2).doubleValue();
            return boxToDouble(val1 + val2);
        }
        throw new NoSuchMethodException();
    }

    /** arg1 - arg2 */
    public static Object subtract(Object arg1, Object arg2) throws NoSuchMethodException {
        int code1 = typeCode(arg1);
        int code2 = typeCode(arg2);
        int maxcode = (code1 < code2) ? code2 : code1;
        if (maxcode <= INT) {
            int val1 = (code1 == CHAR) ? ((Character) arg1).charValue() : ((Number) arg1).intValue();
            int val2 = (code2 == CHAR) ? ((Character) arg2).charValue() : ((Number) arg2).intValue();
            return boxToInteger(val1 - val2);
        }
        if (maxcode <= LONG) {
            long val1 = (code1 == CHAR) ? ((Character) arg1).charValue() : ((Number) arg1).longValue();
            long val2 = (code2 == CHAR) ? ((Character) arg2).charValue() : ((Number) arg2).longValue();
            return boxToLong(val1 - val2);
        }
        if (maxcode <= FLOAT) {
            float val1 = (code1 == CHAR) ? ((Character) arg1).charValue() : ((Number) arg1).floatValue();
            float val2 = (code2 == CHAR) ? ((Character) arg2).charValue() : ((Number) arg2).floatValue();
            return boxToFloat(val1 - val2);
        }
        if (maxcode <= DOUBLE) {
            double val1 = (code1 == CHAR) ? ((Character) arg1).charValue() : ((Number) arg1).doubleValue();
            double val2 = (code2 == CHAR) ? ((Character) arg2).charValue() : ((Number) arg2).doubleValue();
            return boxToDouble(val1 - val2);
        }
        throw new NoSuchMethodException();
    }

    /** arg1 * arg2 */
    public static Object multiply(Object arg1, Object arg2) throws NoSuchMethodException {
        int code1 = typeCode(arg1);
        int code2 = typeCode(arg2);
        int maxcode = (code1 < code2) ? code2 : code1;
        if (maxcode <= INT) {
            int val1 = (code1 == CHAR) ? ((Character) arg1).charValue() : ((Number) arg1).intValue();
            int val2 = (code2 == CHAR) ? ((Character) arg2).charValue() : ((Number) arg2).intValue();
            return boxToInteger(val1 * val2);
        }
        if (maxcode <= LONG) {
            long val1 = (code1 == CHAR) ? ((Character) arg1).charValue() : ((Number) arg1).longValue();
            long val2 = (code2 == CHAR) ? ((Character) arg2).charValue() : ((Number) arg2).longValue();
            return boxToLong(val1 * val2);
        }
        if (maxcode <= FLOAT) {
            float val1 = (code1 == CHAR) ? ((Character) arg1).charValue() : ((Number) arg1).floatValue();
            float val2 = (code2 == CHAR) ? ((Character) arg2).charValue() : ((Number) arg2).floatValue();
            return boxToFloat(val1 * val2);
        }
        if (maxcode <= DOUBLE) {
            double val1 = (code1 == CHAR) ? ((Character) arg1).charValue() : ((Number) arg1).doubleValue();
            double val2 = (code2 == CHAR) ? ((Character) arg2).charValue() : ((Number) arg2).doubleValue();
            return boxToDouble(val1 * val2);
        }
        throw new NoSuchMethodException();
    }

    /** arg1 / arg2 */
    public static Object divide(Object arg1, Object arg2) throws NoSuchMethodException {
        int code1 = typeCode(arg1);
        int code2 = typeCode(arg2);
        int maxcode = (code1 < code2) ? code2 : code1;
        if (maxcode <= INT) {
            int val1 = (code1 == CHAR) ? ((Character) arg1).charValue() : ((Number) arg1).intValue();
            int val2 = (code2 == CHAR) ? ((Character) arg2).charValue() : ((Number) arg2).intValue();
            return boxToInteger(val1 / val2);
        }
        if (maxcode <= LONG) {
            long val1 = (code1 == CHAR) ? ((Character) arg1).charValue() : ((Number) arg1).longValue();
            long val2 = (code2 == CHAR) ? ((Character) arg2).charValue() : ((Number) arg2).longValue();
            return boxToLong(val1 / val2);
        }
        if (maxcode <= FLOAT) {
            float val1 = (code1 == CHAR) ? ((Character) arg1).charValue() : ((Number) arg1).floatValue();
            float val2 = (code2 == CHAR) ? ((Character) arg2).charValue() : ((Number) arg2).floatValue();
            return boxToFloat(val1 / val2);
        }
        if (maxcode <= DOUBLE) {
            double val1 = (code1 == CHAR) ? ((Character) arg1).charValue() : ((Number) arg1).doubleValue();
            double val2 = (code2 == CHAR) ? ((Character) arg2).charValue() : ((Number) arg2).doubleValue();
            return boxToDouble(val1 / val2);
        }
        throw new NoSuchMethodException();
    }

    /** arg1 % arg2 */
    public static Object takeModulo(Object arg1, Object arg2) throws NoSuchMethodException {
        int code1 = typeCode(arg1);
        int code2 = typeCode(arg2);
        int maxcode = (code1 < code2) ? code2 : code1;
        if (maxcode <= INT) {
            int val1 = (code1 == CHAR) ? ((Character) arg1).charValue() : ((Number) arg1).intValue();
            int val2 = (code2 == CHAR) ? ((Character) arg2).charValue() : ((Number) arg2).intValue();
            return boxToInteger(val1 % val2);
        }
        if (maxcode <= LONG) {
            long val1 = (code1 == CHAR) ? ((Character) arg1).charValue() : ((Number) arg1).longValue();
            long val2 = (code2 == CHAR) ? ((Character) arg2).charValue() : ((Number) arg2).longValue();
            return boxToLong(val1 % val2);
        }
        if (maxcode <= FLOAT) {
            float val1 = (code1 == CHAR) ? ((Character) arg1).charValue() : ((Number) arg1).floatValue();
            float val2 = (code2 == CHAR) ? ((Character) arg2).charValue() : ((Number) arg2).floatValue();
            return boxToFloat(val1 % val2);
        }
        if (maxcode <= DOUBLE) {
            double val1 = (code1 == CHAR) ? ((Character) arg1).charValue() : ((Number) arg1).doubleValue();
            double val2 = (code2 == CHAR) ? ((Character) arg2).charValue() : ((Number) arg2).doubleValue();
            return boxToDouble(val1 % val2);
        }
        throw new NoSuchMethodException();
    }

    /** arg1 >> arg2 */
    public static Object shiftSignedRight(Object arg1, Object arg2) throws NoSuchMethodException {
        int code1 = typeCode(arg1);
        int code2 = typeCode(arg2);
        if (code1 <= INT) {
            int val1 = (code1 == CHAR) ? ((Character) arg1).charValue() : ((Number) arg1).intValue();
            if (code2 <= INT) {
                int val2 = (code2 == CHAR) ? ((Character) arg2).charValue() : ((Number) arg2).intValue();
                return boxToInteger(val1 >> val2);
            }
            if (code2 <= LONG) {
                long val2 = (code2 == CHAR) ? ((Character) arg2).charValue() : ((Number) arg2).longValue();
                return boxToInteger(val1 >> val2);
            }
        }
        if (code1 <= LONG) {
            long val1 = (code1 == CHAR) ? ((Character) arg1).charValue() : ((Number) arg1).longValue();
            if (code2 <= INT) {
                int val2 = (code2 == CHAR) ? ((Character) arg2).charValue() : ((Number) arg2).intValue();
                return boxToLong(val1 >> val2);
            }
            if (code2 <= LONG) {
                long val2 = (code2 == CHAR) ? ((Character) arg2).charValue() : ((Number) arg2).longValue();
                return boxToLong(val1 >> val2);
            }
        }
        throw new NoSuchMethodException();
    }

    /** arg1 << arg2 */
    public static Object shiftSignedLeft(Object arg1, Object arg2) throws NoSuchMethodException {
        int code1 = typeCode(arg1);
        int code2 = typeCode(arg2);
        if (code1 <= INT) {
            int val1 = (code1 == CHAR) ? ((Character) arg1).charValue() : ((Number) arg1).intValue();
            if (code2 <= INT) {
                int val2 = (code2 == CHAR) ? ((Character) arg2).charValue() : ((Number) arg2).intValue();
                return boxToInteger(val1 << val2);
            }
            if (code2 <= LONG) {
                long val2 = (code2 == CHAR) ? ((Character) arg2).charValue() : ((Number) arg2).longValue();
                return boxToInteger(val1 << val2);
            }
        }
        if (code1 <= LONG) {
            long val1 = (code1 == CHAR) ? ((Character) arg1).charValue() : ((Number) arg1).longValue();
            if (code2 <= INT) {
                int val2 = (code2 == CHAR) ? ((Character) arg2).charValue() : ((Number) arg2).intValue();
                return boxToLong(val1 << val2);
            }
            if (code2 <= LONG) {
                long val2 = (code2 == CHAR) ? ((Character) arg2).charValue() : ((Number) arg2).longValue();
                return boxToLong(val1 << val2);
            }
        }
        throw new NoSuchMethodException();
    }

    /** arg1 >>> arg2 */
    public static Object shiftLogicalRight(Object arg1, Object arg2) throws NoSuchMethodException {
        int code1 = typeCode(arg1);
        int code2 = typeCode(arg2);
        if (code1 <= INT) {
            int val1 = (code1 == CHAR) ? ((Character) arg1).charValue() : ((Number) arg1).intValue();
            if (code2 <= INT) {
                int val2 = (code2 == CHAR) ? ((Character) arg2).charValue() : ((Number) arg2).intValue();
                return boxToInteger(val1 >>> val2);
            }
            if (code2 <= LONG) {
                long val2 = (code2 == CHAR) ? ((Character) arg2).charValue() : ((Number) arg2).longValue();
                return boxToInteger(val1 >>> val2);
            }
        }
        if (code1 <= LONG) {
            long val1 = (code1 == CHAR) ? ((Character) arg1).charValue() : ((Number) arg1).longValue();
            if (code2 <= INT) {
                int val2 = (code2 == CHAR) ? ((Character) arg2).charValue() : ((Number) arg2).intValue();
                return boxToLong(val1 >>> val2);
            }
            if (code2 <= LONG) {
                long val2 = (code2 == CHAR) ? ((Character) arg2).charValue() : ((Number) arg2).longValue();
                return boxToLong(val1 >>> val2);
            }
        }
        throw new NoSuchMethodException();
    }

    /** -arg */
    public static Object negate(Object arg) throws NoSuchMethodException {
        int code = typeCode(arg);
        if (code <= INT) {
            int val = (code == CHAR) ? ((Character) arg).charValue() : ((Number) arg).intValue();
            return boxToInteger(-val);
        }
        if (code <= LONG) {
            long val = (code == CHAR) ? ((Character) arg).charValue() : ((Number) arg).longValue();
            return boxToLong(-val);
        }
        if (code <= FLOAT) {
            float val = (code == CHAR) ? ((Character) arg).charValue() : ((Number) arg).floatValue();
            return boxToFloat(-val);
        }
        if (code <= DOUBLE) {
            double val = (code == CHAR) ? ((Character) arg).charValue() : ((Number) arg).doubleValue();
            return boxToDouble(-val);
        }
        throw new NoSuchMethodException();
    }

    /** +arg */
    public static Object positive(Object arg) throws NoSuchMethodException {
        int code = typeCode(arg);
        if (code <= INT) {
            int val = (code == CHAR) ? ((Character) arg).charValue() : ((Number) arg).intValue();
            return boxToInteger(+val);
        }
        if (code <= LONG) {
            long val = (code == CHAR) ? ((Character) arg).charValue() : ((Number) arg).longValue();
            return boxToLong(+val);
        }
        if (code <= FLOAT) {
            float val = (code == CHAR) ? ((Character) arg).charValue() : ((Number) arg).floatValue();
            return boxToFloat(+val);
        }
        if (code <= DOUBLE) {
            double val = (code == CHAR) ? ((Character) arg).charValue() : ((Number) arg).doubleValue();
            return boxToDouble(+val);
        }
        throw new NoSuchMethodException();
    }

    /** arg1 & arg2 */
    public static Object takeAnd(Object arg1, Object arg2) throws NoSuchMethodException {
        if ((arg1 instanceof Boolean) || (arg2 instanceof Boolean)) {
            if (!((arg1 instanceof Boolean) && (arg2 instanceof Boolean))) {
                throw new NoSuchMethodException();
            }
            return boxToBoolean(((Boolean) arg1).booleanValue() & ((Boolean) arg2).booleanValue());
        }
        int code1 = typeCode(arg1);
        int code2 = typeCode(arg2);
        int maxcode = (code1 < code2) ? code2 : code1;
        if (maxcode <= INT) {
            int val1 = (code1 == CHAR) ? ((Character) arg1).charValue() : ((Number) arg1).intValue();
            int val2 = (code2 == CHAR) ? ((Character) arg2).charValue() : ((Number) arg2).intValue();
            return boxToInteger(val1 & val2);
        }
        if (maxcode <= LONG) {
            long val1 = (code1 == CHAR) ? ((Character) arg1).charValue() : ((Number) arg1).longValue();
            long val2 = (code2 == CHAR) ? ((Character) arg2).charValue() : ((Number) arg2).longValue();
            return boxToLong(val1 & val2);
        }
        throw new NoSuchMethodException();
    }

    /** arg1 | arg2 */
    public static Object takeOr(Object arg1, Object arg2) throws NoSuchMethodException {
        if ((arg1 instanceof Boolean) || (arg2 instanceof Boolean)) {
            if (!((arg1 instanceof Boolean) && (arg2 instanceof Boolean))) {
                throw new NoSuchMethodException();
            }
            return boxToBoolean(((Boolean) arg1).booleanValue() | ((Boolean) arg2).booleanValue());
        }
        int code1 = typeCode(arg1);
        int code2 = typeCode(arg2);
        int maxcode = (code1 < code2) ? code2 : code1;
        if (maxcode <= INT) {
            int val1 = (code1 == CHAR) ? ((Character) arg1).charValue() : ((Number) arg1).intValue();
            int val2 = (code2 == CHAR) ? ((Character) arg2).charValue() : ((Number) arg2).intValue();
            return boxToInteger(val1 | val2);
        }
        if (maxcode <= LONG) {
            long val1 = (code1 == CHAR) ? ((Character) arg1).charValue() : ((Number) arg1).longValue();
            long val2 = (code2 == CHAR) ? ((Character) arg2).charValue() : ((Number) arg2).longValue();
            return boxToLong(val1 | val2);
        }
        throw new NoSuchMethodException();
    }

    /** arg1 ^ arg2 */
    public static Object takeXor(Object arg1, Object arg2) throws NoSuchMethodException {
        if ((arg1 instanceof Boolean) || (arg2 instanceof Boolean)) {
            if (!((arg1 instanceof Boolean) && (arg2 instanceof Boolean))) {
                throw new NoSuchMethodException();
            }
            return boxToBoolean(((Boolean) arg1).booleanValue() ^ ((Boolean) arg2).booleanValue());
        }
        int code1 = typeCode(arg1);
        int code2 = typeCode(arg2);
        int maxcode = (code1 < code2) ? code2 : code1;
        if (maxcode <= INT) {
            int val1 = (code1 == CHAR) ? ((Character) arg1).charValue() : ((Number) arg1).intValue();
            int val2 = (code2 == CHAR) ? ((Character) arg2).charValue() : ((Number) arg2).intValue();
            return boxToInteger(val1 ^ val2);
        }
        if (maxcode <= LONG) {
            long val1 = (code1 == CHAR) ? ((Character) arg1).charValue() : ((Number) arg1).longValue();
            long val2 = (code2 == CHAR) ? ((Character) arg2).charValue() : ((Number) arg2).longValue();
            return boxToLong(val1 ^ val2);
        }
        throw new NoSuchMethodException();
    }

    /** arg1 && arg2 */
    public static Object takeConditionalAnd(Object arg1, Object arg2) throws NoSuchMethodException {
        if ((arg1 instanceof Boolean) && (arg2 instanceof Boolean)) {
            return boxToBoolean(((Boolean) arg1).booleanValue() && ((Boolean) arg2).booleanValue());
        }
        throw new NoSuchMethodException();
    }

    /** arg1 || arg2 */
    public static Object takeConditionalOr(Object arg1, Object arg2) throws NoSuchMethodException {
        if ((arg1 instanceof Boolean) && (arg2 instanceof Boolean)) {
            return boxToBoolean(((Boolean) arg1).booleanValue() || ((Boolean) arg2).booleanValue());
        }
        throw new NoSuchMethodException();
    }

    /** ~arg */
    public static Object complement(Object arg) throws NoSuchMethodException {
        int code = typeCode(arg);
        if (code <= INT) {
            int val = (code == CHAR) ? ((Character) arg).charValue() : ((Number) arg).intValue();
            return boxToInteger(~val);
        }
        if (code <= LONG) {
            long val = (code == CHAR) ? ((Character) arg).charValue() : ((Number) arg).longValue();
            return boxToLong(~val);
        }
        throw new NoSuchMethodException();
    }

    /** !arg */
    public static Object takeNot(Object arg) throws NoSuchMethodException {
        if (arg instanceof Boolean) {
          return boxToBoolean(!((Boolean) arg).booleanValue());
        }
        throw new NoSuchMethodException();
    }

    public static Object testEqual(Object arg1, Object arg2) throws NoSuchMethodException {
        return boxToBoolean(arg1 == arg2);
    }

    public static Object testNotEqual(Object arg1, Object arg2) throws NoSuchMethodException {
        return boxToBoolean(arg1 != arg2);
    }

    public static Object testLessThan(Object arg1, Object arg2) throws NoSuchMethodException {
        int code1 = typeCode(arg1);
        int code2 = typeCode(arg2);
        int maxcode = (code1 < code2) ? code2 : code1;
        if (maxcode <= INT) {
            int val1 = (code1 == CHAR) ? ((Character) arg1).charValue() : ((Number) arg1).intValue();
            int val2 = (code2 == CHAR) ? ((Character) arg2).charValue() : ((Number) arg2).intValue();
            return boxToBoolean(val1 < val2);
        }
        if (maxcode <= LONG) {
            long val1 = (code1 == CHAR) ? ((Character) arg1).charValue() : ((Number) arg1).longValue();
            long val2 = (code2 == CHAR) ? ((Character) arg2).charValue() : ((Number) arg2).longValue();
            return boxToBoolean(val1 < val2);
        }
        if (maxcode <= FLOAT) {
            float val1 = (code1 == CHAR) ? ((Character) arg1).charValue() : ((Number) arg1).floatValue();
            float val2 = (code2 == CHAR) ? ((Character) arg2).charValue() : ((Number) arg2).floatValue();
            return boxToBoolean(val1 < val2);
        }
        if (maxcode <= DOUBLE) {
            double val1 = (code1 == CHAR) ? ((Character) arg1).charValue() : ((Number) arg1).doubleValue();
            double val2 = (code2 == CHAR) ? ((Character) arg2).charValue() : ((Number) arg2).doubleValue();
            return boxToBoolean(val1 < val2);
        }
        throw new NoSuchMethodException();
    }

    public static Object testLessOrEqualThan(Object arg1, Object arg2) throws NoSuchMethodException {
        int code1 = typeCode(arg1);
        int code2 = typeCode(arg2);
        int maxcode = (code1 < code2) ? code2 : code1;
        if (maxcode <= INT) {
            int val1 = (code1 == CHAR) ? ((Character) arg1).charValue() : ((Number) arg1).intValue();
            int val2 = (code2 == CHAR) ? ((Character) arg2).charValue() : ((Number) arg2).intValue();
            return boxToBoolean(val1 <= val2);
        }
        if (maxcode <= LONG) {
            long val1 = (code1 == CHAR) ? ((Character) arg1).charValue() : ((Number) arg1).longValue();
            long val2 = (code2 == CHAR) ? ((Character) arg2).charValue() : ((Number) arg2).longValue();
            return boxToBoolean(val1 <= val2);
        }
        if (maxcode <= FLOAT) {
            float val1 = (code1 == CHAR) ? ((Character) arg1).charValue() : ((Number) arg1).floatValue();
            float val2 = (code2 == CHAR) ? ((Character) arg2).charValue() : ((Number) arg2).floatValue();
            return boxToBoolean(val1 <= val2);
        }
        if (maxcode <= DOUBLE) {
            double val1 = (code1 == CHAR) ? ((Character) arg1).charValue() : ((Number) arg1).doubleValue();
            double val2 = (code2 == CHAR) ? ((Character) arg2).charValue() : ((Number) arg2).doubleValue();
            return boxToBoolean(val1 <= val2);
        }
        throw new NoSuchMethodException();
    }

    public static Object testGreaterOrEqualThan(Object arg1, Object arg2) throws NoSuchMethodException {
        int code1 = typeCode(arg1);
        int code2 = typeCode(arg2);
        int maxcode = (code1 < code2) ? code2 : code1;
        if (maxcode <= INT) {
            int val1 = (code1 == CHAR) ? ((Character) arg1).charValue() : ((Number) arg1).intValue();
            int val2 = (code2 == CHAR) ? ((Character) arg2).charValue() : ((Number) arg2).intValue();
            return boxToBoolean(val1 >= val2);
        }
        if (maxcode <= LONG) {
            long val1 = (code1 == CHAR) ? ((Character) arg1).charValue() : ((Number) arg1).longValue();
            long val2 = (code2 == CHAR) ? ((Character) arg2).charValue() : ((Number) arg2).longValue();
            return boxToBoolean(val1 >= val2);
        }
        if (maxcode <= FLOAT) {
            float val1 = (code1 == CHAR) ? ((Character) arg1).charValue() : ((Number) arg1).floatValue();
            float val2 = (code2 == CHAR) ? ((Character) arg2).charValue() : ((Number) arg2).floatValue();
            return boxToBoolean(val1 >= val2);
        }
        if (maxcode <= DOUBLE) {
            double val1 = (code1 == CHAR) ? ((Character) arg1).charValue() : ((Number) arg1).doubleValue();
            double val2 = (code2 == CHAR) ? ((Character) arg2).charValue() : ((Number) arg2).doubleValue();
            return boxToBoolean(val1 >= val2);
        }
        throw new NoSuchMethodException();
    }

    public static Object testGreaterThan(Object arg1, Object arg2) throws NoSuchMethodException {
        int code1 = typeCode(arg1);
        int code2 = typeCode(arg2);
        int maxcode = (code1 < code2) ? code2 : code1;
        if (maxcode <= INT) {
            int val1 = (code1 == CHAR) ? ((Character) arg1).charValue() : ((Number) arg1).intValue();
            int val2 = (code2 == CHAR) ? ((Character) arg2).charValue() : ((Number) arg2).intValue();
            return boxToBoolean(val1 > val2);
        }
        if (maxcode <= LONG) {
            long val1 = (code1 == CHAR) ? ((Character) arg1).charValue() : ((Number) arg1).longValue();
            long val2 = (code2 == CHAR) ? ((Character) arg2).charValue() : ((Number) arg2).longValue();
            return boxToBoolean(val1 > val2);
        }
        if (maxcode <= FLOAT) {
            float val1 = (code1 == CHAR) ? ((Character) arg1).charValue() : ((Number) arg1).floatValue();
            float val2 = (code2 == CHAR) ? ((Character) arg2).charValue() : ((Number) arg2).floatValue();
            return boxToBoolean(val1 > val2);
        }
        if (maxcode <= DOUBLE) {
            double val1 = (code1 == CHAR) ? ((Character) arg1).charValue() : ((Number) arg1).doubleValue();
            double val2 = (code2 == CHAR) ? ((Character) arg2).charValue() : ((Number) arg2).doubleValue();
            return boxToBoolean(val1 > val2);
        }
        throw new NoSuchMethodException();
    }

    /** arg.toChar */
    public static Character toCharacter(Object arg) throws NoSuchMethodException {
        if (arg instanceof Character) return (Character)arg;
        if (arg instanceof Byte) return boxToCharacter((char)unboxToByte(arg));
        if (arg instanceof Short) return boxToCharacter((char)unboxToShort(arg));
        if (arg instanceof Integer) return boxToCharacter((char)unboxToInt(arg));
        if (arg instanceof Long) return boxToCharacter((char)unboxToLong(arg));
        if (arg instanceof Float) return boxToCharacter((char)unboxToFloat(arg));
        if (arg instanceof Double) return boxToCharacter((char)unboxToDouble(arg));
        throw new NoSuchMethodException();
    }

    /** arg.toByte */
    public static Byte toByte(Object arg) throws NoSuchMethodException {
        if (arg instanceof Character) return boxToByte((byte)unboxToChar(arg));
        if (arg instanceof Byte) return (Byte)arg;
        if (arg instanceof Short) return boxToByte((byte)unboxToShort(arg));
        if (arg instanceof Integer) return boxToByte((byte)unboxToInt(arg));
        if (arg instanceof Long) return boxToByte((byte)unboxToLong(arg));
        if (arg instanceof Float) return boxToByte((byte)unboxToFloat(arg));
        if (arg instanceof Double) return boxToByte((byte)unboxToDouble(arg));
        throw new NoSuchMethodException();
    }

    /** arg.toShort */
    public static Short toShort(Object arg) throws NoSuchMethodException {
        if (arg instanceof Character) return boxToShort((short)unboxToChar(arg));
        if (arg instanceof Byte) return boxToShort((short)unboxToByte(arg));
        if (arg instanceof Short) return (Short)arg;
        if (arg instanceof Integer) return boxToShort((short)unboxToInt(arg));
        if (arg instanceof Long) return boxToShort((short)unboxToLong(arg));
        if (arg instanceof Float) return boxToShort((short)unboxToFloat(arg));
        if (arg instanceof Double) return boxToShort((short)unboxToDouble(arg));
        throw new NoSuchMethodException();
    }

    /** arg.toInt */
    public static Integer toInteger(Object arg) throws NoSuchMethodException {
        if (arg instanceof Character) return boxToInteger((int)unboxToChar(arg));
        if (arg instanceof Byte) return boxToInteger((int)unboxToByte(arg));
        if (arg instanceof Short) return boxToInteger((int)unboxToShort(arg));
        if (arg instanceof Integer) return (Integer)arg;
        if (arg instanceof Long) return boxToInteger((int)unboxToLong(arg));
        if (arg instanceof Float) return boxToInteger((int)unboxToFloat(arg));
        if (arg instanceof Double) return boxToInteger((int)unboxToDouble(arg));
        throw new NoSuchMethodException();
    }

    /** arg.toLong */
    public static Long toLong(Object arg) throws NoSuchMethodException {
        if (arg instanceof Character) return boxToLong((long)unboxToChar(arg));
        if (arg instanceof Byte) return boxToLong((long)unboxToByte(arg));
        if (arg instanceof Short) return boxToLong((long)unboxToShort(arg));
        if (arg instanceof Integer) return boxToLong((long)unboxToInt(arg));
        if (arg instanceof Long) return (Long)arg;
        if (arg instanceof Float) return boxToLong((long)unboxToFloat(arg));
        if (arg instanceof Double) return boxToLong((long)unboxToDouble(arg));
        throw new NoSuchMethodException();
    }

    /** arg.toFloat */
    public static Float toFloat(Object arg) throws NoSuchMethodException {
        if (arg instanceof Character) return boxToFloat((float)unboxToChar(arg));
        if (arg instanceof Byte) return boxToFloat((float)unboxToByte(arg));
        if (arg instanceof Short) return boxToFloat((float)unboxToShort(arg));
        if (arg instanceof Integer) return boxToFloat((float)unboxToInt(arg));
        if (arg instanceof Long) return boxToFloat((float)unboxToLong(arg));
        if (arg instanceof Float) return (Float)arg;
        if (arg instanceof Double) return boxToFloat((float)unboxToDouble(arg));
        throw new NoSuchMethodException();
    }

    /** arg.toDouble */
    public static Double toDouble(Object arg) throws NoSuchMethodException {
        if (arg instanceof Character) return boxToDouble((double)unboxToChar(arg));
        if (arg instanceof Byte) return boxToDouble((double)unboxToByte(arg));
        if (arg instanceof Short) return boxToDouble((double)unboxToShort(arg));
        if (arg instanceof Integer) return boxToDouble((double)unboxToInt(arg));
        if (arg instanceof Long) return boxToDouble((double)unboxToLong(arg));
        if (arg instanceof Float) return boxToDouble((double)unboxToFloat(arg));
        if (arg instanceof Double) return (Double)arg;
        throw new NoSuchMethodException();
    }

}
