/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.runtime;

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
  */
public final class BoxesRunTime
{
    private static final int CHAR = 0, /* BYTE = 1, SHORT = 2, */ INT = 3, LONG = 4, FLOAT = 5, DOUBLE = 6, OTHER = 7;

    /** We don't need to return BYTE and SHORT, as everything which might
     *  care widens to INT.
     */
    private static int typeCode(Object a) {
        if (a instanceof java.lang.Integer) return INT;
        if (a instanceof java.lang.Double) return DOUBLE;
        if (a instanceof java.lang.Long) return LONG;
        if (a instanceof java.lang.Character) return CHAR;
        if (a instanceof java.lang.Float) return FLOAT;
        if ((a instanceof java.lang.Byte) || (a instanceof java.lang.Short)) return INT;
        return OTHER;
    }

/* BOXING ... BOXING ... BOXING ... BOXING ... BOXING ... BOXING ... BOXING ... BOXING */

    public static java.lang.Boolean boxToBoolean(boolean b) {
        return java.lang.Boolean.valueOf(b);
    }

    public static java.lang.Character boxToCharacter(char c) {
        return java.lang.Character.valueOf(c);
    }

    public static java.lang.Byte boxToByte(byte b) {
        return java.lang.Byte.valueOf(b);
    }

    public static java.lang.Short boxToShort(short s) {
        return java.lang.Short.valueOf(s);
    }

    public static java.lang.Integer boxToInteger(int i) {
        return java.lang.Integer.valueOf(i);
    }

    public static java.lang.Long boxToLong(long l) {
        return java.lang.Long.valueOf(l);
    }

    public static java.lang.Float boxToFloat(float f) {
        return java.lang.Float.valueOf(f);
    }

    public static java.lang.Double boxToDouble(double d) {
        // System.out.println("box " + d);
        // (new Throwable()).printStackTrace();
        return java.lang.Double.valueOf(d);
    }

/* UNBOXING ... UNBOXING ... UNBOXING ... UNBOXING ... UNBOXING ... UNBOXING ... UNBOXING */

    public static boolean unboxToBoolean(Object b) {
        return b == null ? false : ((java.lang.Boolean)b).booleanValue();
    }

    public static char unboxToChar(Object c) {
        return c == null ? 0 : ((java.lang.Character)c).charValue();
    }

    public static byte unboxToByte(Object b) {
        return b == null ? 0 : ((java.lang.Byte)b).byteValue();
    }

    public static short unboxToShort(Object s) {
        return s == null ? 0 : ((java.lang.Short)s).shortValue();
    }

    public static int unboxToInt(Object i) {
        return i == null ? 0 : ((java.lang.Integer)i).intValue();
    }

    public static long unboxToLong(Object l) {
        return l == null ? 0 : ((java.lang.Long)l).longValue();
    }

    public static float unboxToFloat(Object f) {
        return f == null ? 0.0f : ((java.lang.Float)f).floatValue();
    }

    public static double unboxToDouble(Object d) {
        //        System.out.println("unbox " + d);
        return d == null ? 0.0d : ((java.lang.Double)d).doubleValue();
    }

    /* COMPARISON ... COMPARISON ... COMPARISON ... COMPARISON ... COMPARISON ... COMPARISON */

    public static boolean equals(Object x, Object y) {
        if (x == y) return true;
        return equals2(x, y);
    }

    /** Since all applicable logic has to be present in the equals method of a ScalaNumber
     *  in any case, we dispatch to it as soon as we spot one on either side.
     */
    public static boolean equals2(Object x, Object y) {
        if (x instanceof java.lang.Number)
            return equalsNumObject((java.lang.Number)x, y);
        if (x instanceof java.lang.Character)
            return equalsCharObject((java.lang.Character)x, y);
        if (x == null)
            return y == null;

        return x.equals(y);
    }

    public static boolean equalsNumObject(java.lang.Number xn, Object y) {
        if (y instanceof java.lang.Number)
            return equalsNumNum(xn, (java.lang.Number)y);
        if (y instanceof java.lang.Character)
            return equalsNumChar(xn, (java.lang.Character)y);
        if (xn == null)
            return y == null;

        return xn.equals(y);
    }

    public static boolean equalsNumNum(java.lang.Number xn, java.lang.Number yn) {
        int xcode = typeCode(xn);
        int ycode = typeCode(yn);
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
                return yn.equals(xn);
        }
        if (xn == null)
            return yn == null;

        return xn.equals(yn);
    }

    public static boolean equalsCharObject(java.lang.Character xc, Object y) {
        if (y instanceof java.lang.Character)
            return xc.charValue() == ((java.lang.Character)y).charValue();
        if (y instanceof java.lang.Number)
            return equalsNumChar((java.lang.Number)y, xc);
        if (xc == null)
            return y == null;

        return xc.equals(y);
    }

    public static boolean equalsNumChar(java.lang.Number xn, java.lang.Character yc) {
        if (yc == null)
            return xn == null;

        char ch = yc.charValue();
        switch (typeCode(xn)) {
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

    private static int unboxCharOrInt(Object arg1, int code) {
      if (code == CHAR)
        return ((java.lang.Character) arg1).charValue();
      else
        return ((java.lang.Number) arg1).intValue();
    }
    private static long unboxCharOrLong(Object arg1, int code) {
      if (code == CHAR)
        return ((java.lang.Character) arg1).charValue();
      else
        return ((java.lang.Number) arg1).longValue();
    }
    private static float unboxCharOrFloat(Object arg1, int code) {
      if (code == CHAR)
        return ((java.lang.Character) arg1).charValue();
      else
        return ((java.lang.Number) arg1).floatValue();
    }
    private static double unboxCharOrDouble(Object arg1, int code) {
      if (code == CHAR)
        return ((java.lang.Character) arg1).charValue();
      else
        return ((java.lang.Number) arg1).doubleValue();
    }

/* OPERATORS ... OPERATORS ... OPERATORS ... OPERATORS ... OPERATORS ... OPERATORS ... OPERATORS ... OPERATORS */

    /** arg1 + arg2 */
    public static Object add(Object arg1, Object arg2) throws NoSuchMethodException {
        int code1 = typeCode(arg1);
        int code2 = typeCode(arg2);
        int maxcode = (code1 < code2) ? code2 : code1;
        if (maxcode <= INT) {
            return boxToInteger(unboxCharOrInt(arg1, code1) + unboxCharOrInt(arg2, code2));
        }
        if (maxcode <= LONG) {
            return boxToLong(unboxCharOrLong(arg1, code1) + unboxCharOrLong(arg2, code2));
        }
        if (maxcode <= FLOAT) {
            return boxToFloat(unboxCharOrFloat(arg1, code1) + unboxCharOrFloat(arg2, code2));
        }
        if (maxcode <= DOUBLE) {
            return boxToDouble(unboxCharOrDouble(arg1, code1) + unboxCharOrDouble(arg2, code2));
        }
        throw new NoSuchMethodException();
    }

    /** arg1 - arg2 */
    public static Object subtract(Object arg1, Object arg2) throws NoSuchMethodException {
        int code1 = typeCode(arg1);
        int code2 = typeCode(arg2);
        int maxcode = (code1 < code2) ? code2 : code1;
        if (maxcode <= INT) {
            return boxToInteger(unboxCharOrInt(arg1, code1) - unboxCharOrInt(arg2, code2));
        }
        if (maxcode <= LONG) {
            return boxToLong(unboxCharOrLong(arg1, code1) - unboxCharOrLong(arg2, code2));
        }
        if (maxcode <= FLOAT) {
            return boxToFloat(unboxCharOrFloat(arg1, code1) - unboxCharOrFloat(arg2, code2));
        }
        if (maxcode <= DOUBLE) {
            return boxToDouble(unboxCharOrDouble(arg1, code1) - unboxCharOrDouble(arg2, code2));
        }
        throw new NoSuchMethodException();
    }

    /** arg1 * arg2 */
    public static Object multiply(Object arg1, Object arg2) throws NoSuchMethodException {
        int code1 = typeCode(arg1);
        int code2 = typeCode(arg2);
        int maxcode = (code1 < code2) ? code2 : code1;
        if (maxcode <= INT) {
            return boxToInteger(unboxCharOrInt(arg1, code1) * unboxCharOrInt(arg2, code2));
        }
        if (maxcode <= LONG) {
            return boxToLong(unboxCharOrLong(arg1, code1) * unboxCharOrLong(arg2, code2));
        }
        if (maxcode <= FLOAT) {
            return boxToFloat(unboxCharOrFloat(arg1, code1) * unboxCharOrFloat(arg2, code2));
        }
        if (maxcode <= DOUBLE) {
            return boxToDouble(unboxCharOrDouble(arg1, code1) * unboxCharOrDouble(arg2, code2));
        }
        throw new NoSuchMethodException();
    }

    /** arg1 / arg2 */
    public static Object divide(Object arg1, Object arg2) throws NoSuchMethodException {
        int code1 = typeCode(arg1);
        int code2 = typeCode(arg2);
        int maxcode = (code1 < code2) ? code2 : code1;

        if (maxcode <= INT)
            return boxToInteger(unboxCharOrInt(arg1, code1) / unboxCharOrInt(arg2, code2));
        if (maxcode <= LONG)
            return boxToLong(unboxCharOrLong(arg1, code1) / unboxCharOrLong(arg2, code2));
        if (maxcode <= FLOAT)
            return boxToFloat(unboxCharOrFloat(arg1, code1) / unboxCharOrFloat(arg2, code2));
        if (maxcode <= DOUBLE)
            return boxToDouble(unboxCharOrDouble(arg1, code1) / unboxCharOrDouble(arg2, code2));

        throw new NoSuchMethodException();
    }

    /** arg1 % arg2 */
    public static Object takeModulo(Object arg1, Object arg2) throws NoSuchMethodException {
        int code1 = typeCode(arg1);
        int code2 = typeCode(arg2);
        int maxcode = (code1 < code2) ? code2 : code1;

        if (maxcode <= INT)
            return boxToInteger(unboxCharOrInt(arg1, code1) % unboxCharOrInt(arg2, code2));
        if (maxcode <= LONG)
            return boxToLong(unboxCharOrLong(arg1, code1) % unboxCharOrLong(arg2, code2));
        if (maxcode <= FLOAT)
            return boxToFloat(unboxCharOrFloat(arg1, code1) % unboxCharOrFloat(arg2, code2));
        if (maxcode <= DOUBLE)
            return boxToDouble(unboxCharOrDouble(arg1, code1) % unboxCharOrDouble(arg2, code2));

        throw new NoSuchMethodException();
    }

    /** arg1 >> arg2 */
    public static Object shiftSignedRight(Object arg1, Object arg2) throws NoSuchMethodException {
        int code1 = typeCode(arg1);
        int code2 = typeCode(arg2);
        if (code1 <= INT) {
            int val1 = unboxCharOrInt(arg1, code1);
            if (code2 <= INT) {
                int val2 = unboxCharOrInt(arg2, code2);
                return boxToInteger(val1 >> val2);
            }
            if (code2 <= LONG) {
                long val2 = unboxCharOrLong(arg2, code2);
                return boxToInteger(val1 >> val2);
            }
        }
        if (code1 <= LONG) {
            long val1 = unboxCharOrLong(arg1, code1);
            if (code2 <= INT) {
                int val2 = unboxCharOrInt(arg2, code2);
                return boxToLong(val1 >> val2);
            }
            if (code2 <= LONG) {
                long val2 = unboxCharOrLong(arg2, code2);
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
            int val1 = unboxCharOrInt(arg1, code1);
            if (code2 <= INT) {
                int val2 = unboxCharOrInt(arg2, code2);
                return boxToInteger(val1 << val2);
            }
            if (code2 <= LONG) {
                long val2 = unboxCharOrLong(arg2, code2);
                return boxToInteger(val1 << val2);
            }
        }
        if (code1 <= LONG) {
            long val1 = unboxCharOrLong(arg1, code1);
            if (code2 <= INT) {
                int val2 = unboxCharOrInt(arg2, code2);
                return boxToLong(val1 << val2);
            }
            if (code2 <= LONG) {
                long val2 = unboxCharOrLong(arg2, code2);
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
            int val1 = unboxCharOrInt(arg1, code1);
            if (code2 <= INT) {
                int val2 = unboxCharOrInt(arg2, code2);
                return boxToInteger(val1 >>> val2);
            }
            if (code2 <= LONG) {
                long val2 = unboxCharOrLong(arg2, code2);
                return boxToInteger(val1 >>> val2);
            }
        }
        if (code1 <= LONG) {
            long val1 = unboxCharOrLong(arg1, code1);
            if (code2 <= INT) {
                int val2 = unboxCharOrInt(arg2, code2);
                return boxToLong(val1 >>> val2);
            }
            if (code2 <= LONG) {
                long val2 = unboxCharOrLong(arg2, code2);
                return boxToLong(val1 >>> val2);
            }
        }
        throw new NoSuchMethodException();
    }

    /** -arg */
    public static Object negate(Object arg) throws NoSuchMethodException {
        int code = typeCode(arg);
        if (code <= INT) {
            int val = unboxCharOrInt(arg, code);
            return boxToInteger(-val);
        }
        if (code <= LONG) {
            long val = unboxCharOrLong(arg, code);
            return boxToLong(-val);
        }
        if (code <= FLOAT) {
            float val = unboxCharOrFloat(arg, code);
            return boxToFloat(-val);
        }
        if (code <= DOUBLE) {
            double val = unboxCharOrDouble(arg, code);
            return boxToDouble(-val);
        }
        throw new NoSuchMethodException();
    }

    /** +arg */
    public static Object positive(Object arg) throws NoSuchMethodException {
        int code = typeCode(arg);
        if (code <= INT) {
            return boxToInteger(+unboxCharOrInt(arg, code));
        }
        if (code <= LONG) {
            return boxToLong(+unboxCharOrLong(arg, code));
        }
        if (code <= FLOAT) {
            return boxToFloat(+unboxCharOrFloat(arg, code));
        }
        if (code <= DOUBLE) {
            return boxToDouble(+unboxCharOrDouble(arg, code));
        }
        throw new NoSuchMethodException();
    }

    /** arg1 & arg2 */
    public static Object takeAnd(Object arg1, Object arg2) throws NoSuchMethodException {
        if ((arg1 instanceof Boolean) || (arg2 instanceof Boolean)) {
            if ((arg1 instanceof Boolean) && (arg2 instanceof Boolean))
                return boxToBoolean(((java.lang.Boolean) arg1).booleanValue() & ((java.lang.Boolean) arg2).booleanValue());
            else
                throw new NoSuchMethodException();
        }
        int code1 = typeCode(arg1);
        int code2 = typeCode(arg2);
        int maxcode = (code1 < code2) ? code2 : code1;

        if (maxcode <= INT)
            return boxToInteger(unboxCharOrInt(arg1, code1) & unboxCharOrInt(arg2, code2));
        if (maxcode <= LONG)
            return boxToLong(unboxCharOrLong(arg1, code1) & unboxCharOrLong(arg2, code2));

        throw new NoSuchMethodException();
    }

    /** arg1 | arg2 */
    public static Object takeOr(Object arg1, Object arg2) throws NoSuchMethodException {
        if ((arg1 instanceof Boolean) || (arg2 instanceof Boolean)) {
            if ((arg1 instanceof Boolean) && (arg2 instanceof Boolean))
                return boxToBoolean(((java.lang.Boolean) arg1).booleanValue() | ((java.lang.Boolean) arg2).booleanValue());
            else
                throw new NoSuchMethodException();
        }
        int code1 = typeCode(arg1);
        int code2 = typeCode(arg2);
        int maxcode = (code1 < code2) ? code2 : code1;

        if (maxcode <= INT)
            return boxToInteger(unboxCharOrInt(arg1, code1) | unboxCharOrInt(arg2, code2));
        if (maxcode <= LONG)
            return boxToLong(unboxCharOrLong(arg1, code1) | unboxCharOrLong(arg2, code2));

        throw new NoSuchMethodException();
    }

    /** arg1 ^ arg2 */
    public static Object takeXor(Object arg1, Object arg2) throws NoSuchMethodException {
        if ((arg1 instanceof Boolean) || (arg2 instanceof Boolean)) {
            if ((arg1 instanceof Boolean) && (arg2 instanceof Boolean))
                return boxToBoolean(((java.lang.Boolean) arg1).booleanValue() ^ ((java.lang.Boolean) arg2).booleanValue());
            else
                throw new NoSuchMethodException();
        }
        int code1 = typeCode(arg1);
        int code2 = typeCode(arg2);
        int maxcode = (code1 < code2) ? code2 : code1;

        if (maxcode <= INT)
            return boxToInteger(unboxCharOrInt(arg1, code1) ^ unboxCharOrInt(arg2, code2));
        if (maxcode <= LONG)
            return boxToLong(unboxCharOrLong(arg1, code1) ^ unboxCharOrLong(arg2, code2));

        throw new NoSuchMethodException();
    }

    /** arg1 && arg2 */
    public static Object takeConditionalAnd(Object arg1, Object arg2) throws NoSuchMethodException {
        if ((arg1 instanceof Boolean) && (arg2 instanceof Boolean)) {
            return boxToBoolean(((java.lang.Boolean) arg1).booleanValue() && ((java.lang.Boolean) arg2).booleanValue());
        }
        throw new NoSuchMethodException();
    }

    /** arg1 || arg2 */
    public static Object takeConditionalOr(Object arg1, Object arg2) throws NoSuchMethodException {
        if ((arg1 instanceof Boolean) && (arg2 instanceof Boolean)) {
            return boxToBoolean(((java.lang.Boolean) arg1).booleanValue() || ((java.lang.Boolean) arg2).booleanValue());
        }
        throw new NoSuchMethodException();
    }

    /** ~arg */
    public static Object complement(Object arg) throws NoSuchMethodException {
        int code = typeCode(arg);
        if (code <= INT) {
            return boxToInteger(~unboxCharOrInt(arg, code));
        }
        if (code <= LONG) {
            return boxToLong(~unboxCharOrLong(arg, code));
        }
        throw new NoSuchMethodException();
    }

    /** !arg */
    public static Object takeNot(Object arg) throws NoSuchMethodException {
        if (arg instanceof Boolean) {
          return boxToBoolean(!((java.lang.Boolean) arg).booleanValue());
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
            int val1 = unboxCharOrInt(arg1, code1);
            int val2 = unboxCharOrInt(arg2, code2);
            return boxToBoolean(val1 < val2);
        }
        if (maxcode <= LONG) {
            long val1 = unboxCharOrLong(arg1, code1);
            long val2 = unboxCharOrLong(arg2, code2);
            return boxToBoolean(val1 < val2);
        }
        if (maxcode <= FLOAT) {
            float val1 = unboxCharOrFloat(arg1, code1);
            float val2 = unboxCharOrFloat(arg2, code2);
            return boxToBoolean(val1 < val2);
        }
        if (maxcode <= DOUBLE) {
            double val1 = unboxCharOrDouble(arg1, code1);
            double val2 = unboxCharOrDouble(arg2, code2);
            return boxToBoolean(val1 < val2);
        }
        throw new NoSuchMethodException();
    }

    public static Object testLessOrEqualThan(Object arg1, Object arg2) throws NoSuchMethodException {
        int code1 = typeCode(arg1);
        int code2 = typeCode(arg2);
        int maxcode = (code1 < code2) ? code2 : code1;
        if (maxcode <= INT) {
            int val1 = unboxCharOrInt(arg1, code1);
            int val2 = unboxCharOrInt(arg2, code2);
            return boxToBoolean(val1 <= val2);
        }
        if (maxcode <= LONG) {
            long val1 = unboxCharOrLong(arg1, code1);
            long val2 = unboxCharOrLong(arg2, code2);
            return boxToBoolean(val1 <= val2);
        }
        if (maxcode <= FLOAT) {
            float val1 = unboxCharOrFloat(arg1, code1);
            float val2 = unboxCharOrFloat(arg2, code2);
            return boxToBoolean(val1 <= val2);
        }
        if (maxcode <= DOUBLE) {
            double val1 = unboxCharOrDouble(arg1, code1);
            double val2 = unboxCharOrDouble(arg2, code2);
            return boxToBoolean(val1 <= val2);
        }
        throw new NoSuchMethodException();
    }

    public static Object testGreaterOrEqualThan(Object arg1, Object arg2) throws NoSuchMethodException {
        int code1 = typeCode(arg1);
        int code2 = typeCode(arg2);
        int maxcode = (code1 < code2) ? code2 : code1;
        if (maxcode <= INT) {
            int val1 = unboxCharOrInt(arg1, code1);
            int val2 = unboxCharOrInt(arg2, code2);
            return boxToBoolean(val1 >= val2);
        }
        if (maxcode <= LONG) {
            long val1 = unboxCharOrLong(arg1, code1);
            long val2 = unboxCharOrLong(arg2, code2);
            return boxToBoolean(val1 >= val2);
        }
        if (maxcode <= FLOAT) {
            float val1 = unboxCharOrFloat(arg1, code1);
            float val2 = unboxCharOrFloat(arg2, code2);
            return boxToBoolean(val1 >= val2);
        }
        if (maxcode <= DOUBLE) {
            double val1 = unboxCharOrDouble(arg1, code1);
            double val2 = unboxCharOrDouble(arg2, code2);
            return boxToBoolean(val1 >= val2);
        }
        throw new NoSuchMethodException();
    }

    public static Object testGreaterThan(Object arg1, Object arg2) throws NoSuchMethodException {
        int code1 = typeCode(arg1);
        int code2 = typeCode(arg2);
        int maxcode = (code1 < code2) ? code2 : code1;
        if (maxcode <= INT) {
            int val1 = unboxCharOrInt(arg1, code1);
            int val2 = unboxCharOrInt(arg2, code2);
            return boxToBoolean(val1 > val2);
        }
        if (maxcode <= LONG) {
            long val1 = unboxCharOrLong(arg1, code1);
            long val2 = unboxCharOrLong(arg2, code2);
            return boxToBoolean(val1 > val2);
        }
        if (maxcode <= FLOAT) {
            float val1 = unboxCharOrFloat(arg1, code1);
            float val2 = unboxCharOrFloat(arg2, code2);
            return boxToBoolean(val1 > val2);
        }
        if (maxcode <= DOUBLE) {
            double val1 = unboxCharOrDouble(arg1, code1);
            double val2 = unboxCharOrDouble(arg2, code2);
            return boxToBoolean(val1 > val2);
        }
        throw new NoSuchMethodException();
    }

    public static boolean isBoxedNumberOrBoolean(Object arg) {
      return (arg instanceof java.lang.Boolean) || isBoxedNumber(arg);
    }
    public static boolean isBoxedNumber(Object arg) {
      return  (
           (arg instanceof java.lang.Integer)
        || (arg instanceof java.lang.Long)
        || (arg instanceof java.lang.Double)
        || (arg instanceof java.lang.Float)
        || (arg instanceof java.lang.Short)
        || (arg instanceof java.lang.Character)
        || (arg instanceof java.lang.Byte)
      );
    }

    /** arg.toChar */
    public static java.lang.Character toCharacter(Object arg) throws NoSuchMethodException {
        if (arg instanceof java.lang.Integer) return boxToCharacter((char)unboxToInt(arg));
        if (arg instanceof java.lang.Short) return boxToCharacter((char)unboxToShort(arg));
        if (arg instanceof java.lang.Character) return (java.lang.Character)arg;
        if (arg instanceof java.lang.Long) return boxToCharacter((char)unboxToLong(arg));
        if (arg instanceof java.lang.Byte) return boxToCharacter((char)unboxToByte(arg));
        if (arg instanceof java.lang.Float) return boxToCharacter((char)unboxToFloat(arg));
        if (arg instanceof java.lang.Double) return boxToCharacter((char)unboxToDouble(arg));
        throw new NoSuchMethodException();
    }

    /** arg.toByte */
    public static java.lang.Byte toByte(Object arg) throws NoSuchMethodException {
        if (arg instanceof java.lang.Integer) return boxToByte((byte)unboxToInt(arg));
        if (arg instanceof java.lang.Character) return boxToByte((byte)unboxToChar(arg));
        if (arg instanceof java.lang.Byte) return (java.lang.Byte)arg;
        if (arg instanceof java.lang.Long) return boxToByte((byte)unboxToLong(arg));
        if (arg instanceof java.lang.Short) return boxToByte((byte)unboxToShort(arg));
        if (arg instanceof java.lang.Float) return boxToByte((byte)unboxToFloat(arg));
        if (arg instanceof java.lang.Double) return boxToByte((byte)unboxToDouble(arg));
        throw new NoSuchMethodException();
    }

    /** arg.toShort */
    public static java.lang.Short toShort(Object arg) throws NoSuchMethodException {
        if (arg instanceof java.lang.Integer) return boxToShort((short)unboxToInt(arg));
        if (arg instanceof java.lang.Long) return boxToShort((short)unboxToLong(arg));
        if (arg instanceof java.lang.Character) return boxToShort((short)unboxToChar(arg));
        if (arg instanceof java.lang.Byte) return boxToShort((short)unboxToByte(arg));
        if (arg instanceof java.lang.Short) return (java.lang.Short)arg;
        if (arg instanceof java.lang.Float) return boxToShort((short)unboxToFloat(arg));
        if (arg instanceof java.lang.Double) return boxToShort((short)unboxToDouble(arg));
        throw new NoSuchMethodException();
    }

    /** arg.toInt */
    public static java.lang.Integer toInteger(Object arg) throws NoSuchMethodException {
        if (arg instanceof java.lang.Integer) return (java.lang.Integer)arg;
        if (arg instanceof java.lang.Long) return boxToInteger((int)unboxToLong(arg));
        if (arg instanceof java.lang.Double) return boxToInteger((int)unboxToDouble(arg));
        if (arg instanceof java.lang.Float) return boxToInteger((int)unboxToFloat(arg));
        if (arg instanceof java.lang.Character) return boxToInteger((int)unboxToChar(arg));
        if (arg instanceof java.lang.Byte) return boxToInteger((int)unboxToByte(arg));
        if (arg instanceof java.lang.Short) return boxToInteger((int)unboxToShort(arg));
        throw new NoSuchMethodException();
    }

    /** arg.toLong */
    public static java.lang.Long toLong(Object arg) throws NoSuchMethodException {
        if (arg instanceof java.lang.Integer) return boxToLong((long)unboxToInt(arg));
        if (arg instanceof java.lang.Double) return boxToLong((long)unboxToDouble(arg));
        if (arg instanceof java.lang.Float) return boxToLong((long)unboxToFloat(arg));
        if (arg instanceof java.lang.Long) return (java.lang.Long)arg;
        if (arg instanceof java.lang.Character) return boxToLong((long)unboxToChar(arg));
        if (arg instanceof java.lang.Byte) return boxToLong((long)unboxToByte(arg));
        if (arg instanceof java.lang.Short) return boxToLong((long)unboxToShort(arg));
        throw new NoSuchMethodException();
    }

    /** arg.toFloat */
    public static java.lang.Float toFloat(Object arg) throws NoSuchMethodException {
        if (arg instanceof java.lang.Integer) return boxToFloat((float)unboxToInt(arg));
        if (arg instanceof java.lang.Long) return boxToFloat((float)unboxToLong(arg));
        if (arg instanceof java.lang.Float) return (java.lang.Float)arg;
        if (arg instanceof java.lang.Double) return boxToFloat((float)unboxToDouble(arg));
        if (arg instanceof java.lang.Character) return boxToFloat((float)unboxToChar(arg));
        if (arg instanceof java.lang.Byte) return boxToFloat((float)unboxToByte(arg));
        if (arg instanceof java.lang.Short) return boxToFloat((float)unboxToShort(arg));
        throw new NoSuchMethodException();
    }

    /** arg.toDouble */
    public static java.lang.Double toDouble(Object arg) throws NoSuchMethodException {
        if (arg instanceof java.lang.Integer) return boxToDouble((double)unboxToInt(arg));
        if (arg instanceof java.lang.Float) return boxToDouble((double)unboxToFloat(arg));
        if (arg instanceof java.lang.Double) return (java.lang.Double)arg;
        if (arg instanceof java.lang.Long) return boxToDouble((double)unboxToLong(arg));
        if (arg instanceof java.lang.Character) return boxToDouble((double)unboxToChar(arg));
        if (arg instanceof java.lang.Byte) return boxToDouble((double)unboxToByte(arg));
        if (arg instanceof java.lang.Short) return boxToDouble((double)unboxToShort(arg));
        throw new NoSuchMethodException();
    }

}
