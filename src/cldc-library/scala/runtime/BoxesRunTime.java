/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
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

    private static final int CHAR = 0, BYTE = 1, SHORT = 2, INT = 3, LONG = 4, OTHER = 7;

    private static int typeCode(Object a) {
        if (a instanceof Integer) return INT;
        if (a instanceof Character) return CHAR;
        if (a instanceof Long) return LONG;
        if (a instanceof Byte) return BYTE;
        if (a instanceof Short) return SHORT;
        return OTHER;
    }

/* BOXING ... BOXING ... BOXING ... BOXING ... BOXING ... BOXING ... BOXING ... BOXING */

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

    private static final Boolean TRUE = new Boolean(true);
    private static final Boolean FALSE = new Boolean(false);
    public static Boolean boxToBoolean(boolean b) {
        return b ? TRUE : FALSE;
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

/* UNBOXING ... UNBOXING ... UNBOXING ... UNBOXING ... UNBOXING ... UNBOXING ... UNBOXING */

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

/* COMPARISON ... COMPARISON ... COMPARISON ... COMPARISON ... COMPARISON ... COMPARISON */

    /** A rich implementation of the <code>equals</code> method that overrides the
      * default equals because Java's boxed primitives are utterly broken. This equals
      * is inserted instead of a normal equals by the Scala compiler (in the
      * ICode phase, method <code>genEqEqPrimitive</code>) only when either
      * side of the comparison is a subclass of <code>AnyVal</code>, of
      * <code>java.lang.Number</code>, of <code>java.lang.Character</code> or
      * is exactly <code>Any</code> or <code>AnyRef</code>. */
    public static boolean equals(Object a, Object b) {
        if (a == null)
            return b == null;
        if (b == null)
            return false;
        if (a.equals(b))
            return true;

        final long left =
            (a instanceof Integer) ? ((Integer)a).intValue() :
            (a instanceof Character) ? ((Character)a).charValue() :
            (a instanceof Long) ? ((Long)a).longValue() :
            (a instanceof Byte) ? ((Byte)a).byteValue() :
            ((Short)a).shortValue();

        final long right =
            (b instanceof Integer) ? ((Integer)b).intValue() :
            (b instanceof Character) ? ((Character)b).charValue() :
            (b instanceof Long) ? ((Long)b).longValue() :
            (b instanceof Byte) ? ((Byte)b).byteValue() :
            ((Short)b).shortValue();

        return left == right;
    }

/* OPERATORS ... OPERATORS ... OPERATORS ... OPERATORS ... OPERATORS ... OPERATORS ... OPERATORS ... OPERATORS */

    /** arg1 + arg2 */
    public static Object add(Object arg1, Object arg2) throws Error {
        throw new Error();
    }

    /** arg1 - arg2 */
    public static Object subtract(Object arg1, Object arg2) throws Error {
        throw new Error();
    }

    /** arg1 * arg2 */
    public static Object multiply(Object arg1, Object arg2) throws Error {
        throw new Error();
    }

    /** arg1 / arg2 */
    public static Object divide(Object arg1, Object arg2) throws Error {
        throw new Error();
    }

    /** arg1 % arg2 */
    public static Object takeModulo(Object arg1, Object arg2) throws Error {
        throw new Error();
    }

    /** arg1 >> arg2 */
    public static Object shiftSignedRight(Object arg1, Object arg2) throws Error {
        throw new Error();
    }

    /** arg1 << arg2 */
    public static Object shiftSignedLeft(Object arg1, Object arg2) throws Error {
        throw new Error();
    }

    /** arg1 >>> arg2 */
    public static Object shiftLogicalRight(Object arg1, Object arg2) throws Error {
        throw new Error();
    }

    /** -arg */
    public static Object negate(Object arg) throws Error {
        throw new Error();
    }

    /** +arg */
    public static Object positive(Object arg) throws Error {
        throw new Error();
    }

    /** arg1 & arg2 */
    public static Object takeAnd(Object arg1, Object arg2) throws Error {
        throw new Error();
    }

    /** arg1 | arg2 */
    public static Object takeOr(Object arg1, Object arg2) throws Error {
        throw new Error();
    }

    /** arg1 ^ arg2 */
    public static Object takeXor(Object arg1, Object arg2) throws Error {
        throw new Error();
    }

    /** arg1 && arg2 */
    public static Object takeConditionalAnd(Object arg1, Object arg2) throws Error {
        throw new Error();
    }

    /** arg1 || arg2 */
    public static Object takeConditionalOr(Object arg1, Object arg2) throws Error {
        throw new Error();
    }

    /** ~arg */
    public static Object complement(Object arg) throws Error {
        throw new Error();
    }

    /** !arg */
    public static Object takeNot(Object arg) throws Error {
        throw new Error();
    }

    public static Object testEqual(Object arg1, Object arg2) throws Error {
        throw new Error();
    }

    public static Object testNotEqual(Object arg1, Object arg2) throws Error {
        throw new Error();
    }

    public static Object testLessThan(Object arg1, Object arg2) throws Error {
        throw new Error();
    }

    public static Object testLessOrEqualThan(Object arg1, Object arg2) throws Error {
        throw new Error();
    }

    public static Object testGreaterOrEqualThan(Object arg1, Object arg2) throws Error {
        throw new Error();
    }

    public static Object testGreaterThan(Object arg1, Object arg2) throws Error {
        throw new Error();
    }

    /** arg.toChar */
    public static Character toCharacter(Object arg) throws Error {
        throw new Error();
    }

    /** arg.toByte */
    public static Byte toByte(Object arg) throws Error {
        throw new Error();
    }

    /** arg.toShort */
    public static Short toShort(Object arg) throws Error {
        throw new Error();
    }

    /** arg.toInt */
    public static Integer toInteger(Object arg) throws Error {
        throw new Error();
    }

    /** arg.toLong */
    public static Long toLong(Object arg) throws Error {
        throw new Error();
    }

}
