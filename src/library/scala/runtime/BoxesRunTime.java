/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.runtime;

import java.io.*;
import scala.math.ScalaNumber;

/** An object (static class) that defines methods used for creating,
  * reverting, and calculating with, boxed values. There are four classes
  * of methods in this object:
  *   - Convenience boxing methods which call the static valueOf method
  *     on the boxed class, thus utilizing the JVM boxing cache.
  *   - Convenience unboxing methods returning default value on null.
  *   - The generalised comparison method to be used when an object may
  *     be a boxed value.
  *   - Standard value operators for boxed number and quasi-number values.
  *
  * @author  Gilles Dubochet
  * @author  Martin Odersky
  * @contributor Stepan Koltsov
  * @version 2.0 */
public class BoxesRunTime
{
    private static final int CHAR = 0, BYTE = 1, SHORT = 2, INT = 3, LONG = 4, FLOAT = 5, DOUBLE = 6, OTHER = 7;

    private static int typeCode(Object a) {
        if (a instanceof Integer) return INT;
        if (a instanceof Byte) return BYTE;
        if (a instanceof Character) return CHAR;
        if (a instanceof Long) return LONG;
        if (a instanceof Double) return DOUBLE;
        if (a instanceof Short) return SHORT;
        if (a instanceof Float) return FLOAT;
        return OTHER;
    }

    private static String boxDescription(Object a) {
      return "" + a.getClass().getSimpleName() + "(" + a + ")";
    }

/* BOXING ... BOXING ... BOXING ... BOXING ... BOXING ... BOXING ... BOXING ... BOXING */

    public static Boolean boxToBoolean(boolean b) {
        return Boolean.valueOf(b);
    }

    public static Character boxToCharacter(char c) {
        return Character.valueOf(c);
    }

    public static Byte boxToByte(byte b) {
        return Byte.valueOf(b);
    }

    public static Short boxToShort(short s) {
        return Short.valueOf(s);
    }

    public static Integer boxToInteger(int i) {
        return Integer.valueOf(i);
    }

    public static Long boxToLong(long l) {
        return Long.valueOf(l);
    }

    public static Float boxToFloat(float f) {
        return Float.valueOf(f);
    }

    public static Double boxToDouble(double d) {
        return Double.valueOf(d);
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

    /* COMPARISON ... COMPARISON ... COMPARISON ... COMPARISON ... COMPARISON ... COMPARISON */

    private static int eqTypeCode(Number a) {
        if ((a instanceof Integer) || (a instanceof Byte)) return INT;
        if (a instanceof Long) return LONG;
        if (a instanceof Double) return DOUBLE;
        if (a instanceof Short) return INT;
        if (a instanceof Float) return FLOAT;
        return OTHER;
    }

    public static boolean equals(Object x, Object y) {
        if (x == y) return true;
        if (x == null) return false;
        return equals2(x, y);
    }

    /** Since all applicable logic has to be present in the equals method of a ScalaNumber
     *  in any case, we dispatch to it as soon as we spot one on either side.
     */
    public static boolean equals2(Object x, Object y) {
        if (x instanceof Number) {
            Number xn = (Number)x;

            if (y instanceof Number) {
                Number yn = (Number)y;
                int xcode = eqTypeCode(xn);
                int ycode = eqTypeCode(yn);
                switch (ycode > xcode ? ycode : xcode) {
                case INT:
                    return xn.intValue() == yn.intValue();
                case LONG:
                    return xn.longValue() == yn.longValue();
                case FLOAT:
                    return xn.floatValue() == yn.floatValue();
                case DOUBLE:
                    return xn.doubleValue() == yn.doubleValue();
                default:
                    if ((yn instanceof ScalaNumber) && !(xn instanceof ScalaNumber))
                        return y.equals(x);
                }
            } else if (y instanceof Character)
                return equalsNumChar(xn, (Character)y);
        } else if (x instanceof Character) {
            Character xc = (Character)x;
            if (y instanceof Character)
                return xc.charValue() == ((Character)y).charValue();
            if (y instanceof Number)
                return equalsNumChar((Number)y, xc);
        }
        return x.equals(y);
    }

    private static boolean equalsNumChar(Number xn, Character yc) {
        char ch = yc.charValue();
        switch (eqTypeCode(xn)) {
        case INT:
            return xn.intValue() == ch;
        case LONG:
            return xn.longValue() == ch;
        case FLOAT:
            return xn.floatValue() == ch;
        case DOUBLE:
            return xn.doubleValue() == ch;
        default:
            return xn.equals(yc);
        }
    }

    /** Hashcode algorithm is driven by the requirements imposed
     *  by primitive equality semantics, namely that equal objects
     *  have equal hashCodes.  The first priority are the integral/char
     *  types, which already have the same hashCodes for the same
     *  values except for Long.  So Long's hashCode is altered to
     *  conform to Int's for all values in Int's range.
     *
     *  Float is problematic because it's far too small to hold
     *  all the Ints, so for instance Int.MaxValue.toFloat claims
     *  to be == to each of the largest 64 Ints.  There is no way
     *  to preserve equals/hashCode alignment without compromising
     *  the hashCode distribution, so Floats are only guaranteed
     *  to have the same hashCode for whole Floats in the range
     *  Short.MinValue to Short.MaxValue (2^16 total.)
     *
     *  Double has its hashCode altered to match the entire Int range,
     *  but is not guaranteed beyond that.  (But could/should it be?
     *  The hashCode is only 32 bits so this is a more tractable
     *  issue than Float's, but it might be better simply to exclude it.)
     *
     *  Note: BigInt and BigDecimal, being arbitrary precision, could
     *  be made consistent with all other types for the Int range, but
     *  as yet have not.
     *
     *  Note: Among primitives, Float.NaN != Float.NaN, but the boxed
     *  verisons are equal.  This still needs reconciliation.
     */
    public static int hashFromLong(Long n) {
      int iv = n.intValue();
      if (iv == n.longValue()) return iv;
      else return n.hashCode();
    }
    public static int hashFromDouble(Double n) {
      int iv = n.intValue();
      double dv = n.doubleValue();
      if (iv == dv) return iv;

      long lv = n.longValue();
      if (lv == dv) return Long.valueOf(lv).hashCode();
      else return n.hashCode();
    }
    public static int hashFromFloat(Float n) {
      int iv = n.intValue();
      float fv = n.floatValue();
      if (iv == fv) return iv;

      long lv = n.longValue();
      if (lv == fv) return Long.valueOf(lv).hashCode();
      else return n.hashCode();
    }
    public static int hashFromNumber(Number n) {
      if (n instanceof Long) return hashFromLong((Long)n);
      else if (n instanceof Double) return hashFromDouble((Double)n);
      else if (n instanceof Float) return hashFromFloat((Float)n);
      else return n.hashCode();
    }
    public static int hashFromObject(Object a) {
      if (a instanceof Number) return hashFromNumber((Number)a);
      else return a.hashCode();
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
        if (arg instanceof Integer) return boxToCharacter((char)unboxToInt(arg));
        if (arg instanceof Short) return boxToCharacter((char)unboxToShort(arg));
        if (arg instanceof Character) return (Character)arg;
        if (arg instanceof Long) return boxToCharacter((char)unboxToLong(arg));
        if (arg instanceof Byte) return boxToCharacter((char)unboxToByte(arg));
        if (arg instanceof Float) return boxToCharacter((char)unboxToFloat(arg));
        if (arg instanceof Double) return boxToCharacter((char)unboxToDouble(arg));
        throw new NoSuchMethodException();
    }

    /** arg.toByte */
    public static Byte toByte(Object arg) throws NoSuchMethodException {
        if (arg instanceof Integer) return boxToByte((byte)unboxToInt(arg));
        if (arg instanceof Character) return boxToByte((byte)unboxToChar(arg));
        if (arg instanceof Byte) return (Byte)arg;
        if (arg instanceof Long) return boxToByte((byte)unboxToLong(arg));
        if (arg instanceof Short) return boxToByte((byte)unboxToShort(arg));
        if (arg instanceof Float) return boxToByte((byte)unboxToFloat(arg));
        if (arg instanceof Double) return boxToByte((byte)unboxToDouble(arg));
        throw new NoSuchMethodException();
    }

    /** arg.toShort */
    public static Short toShort(Object arg) throws NoSuchMethodException {
        if (arg instanceof Integer) return boxToShort((short)unboxToInt(arg));
        if (arg instanceof Long) return boxToShort((short)unboxToLong(arg));
        if (arg instanceof Character) return boxToShort((short)unboxToChar(arg));
        if (arg instanceof Byte) return boxToShort((short)unboxToByte(arg));
        if (arg instanceof Short) return (Short)arg;
        if (arg instanceof Float) return boxToShort((short)unboxToFloat(arg));
        if (arg instanceof Double) return boxToShort((short)unboxToDouble(arg));
        throw new NoSuchMethodException();
    }

    /** arg.toInt */
    public static Integer toInteger(Object arg) throws NoSuchMethodException {
        if (arg instanceof Integer) return (Integer)arg;
        if (arg instanceof Long) return boxToInteger((int)unboxToLong(arg));
        if (arg instanceof Double) return boxToInteger((int)unboxToDouble(arg));
        if (arg instanceof Float) return boxToInteger((int)unboxToFloat(arg));
        if (arg instanceof Character) return boxToInteger((int)unboxToChar(arg));
        if (arg instanceof Byte) return boxToInteger((int)unboxToByte(arg));
        if (arg instanceof Short) return boxToInteger((int)unboxToShort(arg));
        throw new NoSuchMethodException();
    }

    /** arg.toLong */
    public static Long toLong(Object arg) throws NoSuchMethodException {
        if (arg instanceof Integer) return boxToLong((long)unboxToInt(arg));
        if (arg instanceof Double) return boxToLong((long)unboxToDouble(arg));
        if (arg instanceof Float) return boxToLong((long)unboxToFloat(arg));
        if (arg instanceof Long) return (Long)arg;
        if (arg instanceof Character) return boxToLong((long)unboxToChar(arg));
        if (arg instanceof Byte) return boxToLong((long)unboxToByte(arg));
        if (arg instanceof Short) return boxToLong((long)unboxToShort(arg));
        throw new NoSuchMethodException();
    }

    /** arg.toFloat */
    public static Float toFloat(Object arg) throws NoSuchMethodException {
        if (arg instanceof Integer) return boxToFloat((float)unboxToInt(arg));
        if (arg instanceof Long) return boxToFloat((float)unboxToLong(arg));
        if (arg instanceof Float) return (Float)arg;
        if (arg instanceof Double) return boxToFloat((float)unboxToDouble(arg));
        if (arg instanceof Character) return boxToFloat((float)unboxToChar(arg));
        if (arg instanceof Byte) return boxToFloat((float)unboxToByte(arg));
        if (arg instanceof Short) return boxToFloat((float)unboxToShort(arg));
        throw new NoSuchMethodException();
    }

    /** arg.toDouble */
    public static Double toDouble(Object arg) throws NoSuchMethodException {
        if (arg instanceof Integer) return boxToDouble((double)unboxToInt(arg));
        if (arg instanceof Float) return boxToDouble((double)unboxToFloat(arg));
        if (arg instanceof Double) return (Double)arg;
        if (arg instanceof Long) return boxToDouble((double)unboxToLong(arg));
        if (arg instanceof Character) return boxToDouble((double)unboxToChar(arg));
        if (arg instanceof Byte) return boxToDouble((double)unboxToByte(arg));
        if (arg instanceof Short) return boxToDouble((double)unboxToShort(arg));
        throw new NoSuchMethodException();
    }

}
