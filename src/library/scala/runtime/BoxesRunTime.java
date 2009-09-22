/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.runtime;

import java.io.*;

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
    /**** Temporary code to support logging all equality comparisons. ****/
    private static boolean eqeqLogging = false;
    private static String eqeqLogName = "/tmp/trunk-eqeq.log";
    private static FileWriter eqeqLog;
    public static void setEqEqLogging(boolean state) {
      eqeqLogging = state;
      if (state) {
        try { eqeqLog = new FileWriter(eqeqLogName, true); }
        catch (IOException e) { eqeqLog = null; }

        log("Started eqeq log at " + (new java.util.Date()));
      }
    }
    private static String obToString(Object o) {
      String s = o.toString() + " (" + o.getClass().getSimpleName() + ")";
      return s.replaceAll("\\n", " ");
    }
    private static void logInternal(String msg, Object a, Object b, String where) {
      log(msg + obToString(a) + " == " + obToString(b) + " " + where);
    }

    public static String whereAreWe() {
      StackTraceElement e = Thread.currentThread().getStackTrace()[3];
      return"(" + e.getClassName() + "." + e.getMethodName() + e.getFileName() + ":" + e.getLineNumber() + ")";
    }
    public static void log(String msg) {
      if (eqeqLogging && eqeqLog != null) {
        try {
          eqeqLog.write(msg + "\n");
          eqeqLog.flush();
        }
        catch (IOException e) { }
      }
    }

    /**** End temporary logging section. ****/

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

    public static Boolean boxToBoolean(boolean b) {
        return Boolean.valueOf(b);
    }

    public static Character boxToCharacter(char c) {
        // !!! Temporarily working around the "impossible" (?) fact that
        // c can have a negative value here.  In any revision since r17461 try:
        //   def foo = new (Short => Char) { def apply(x: Short) = x.toChar }
        //   foo(-100)
        // and the -100 will get to Character, which will duly crash.
        // The bug was masked before because the Characters were created
        // with "new Character(c)" and the constructor avenue must have
        // some check against negative values, whereas the static method doesn't.
        //
        // It appears to be Short-specific; I can't get anything similar
        // out of Byte or Int.
        return Character.valueOf((char)(c & 0xFFFF));
        // return new Character(c); <-- this also would work
        // return Character.valueOf(c); <-- but not this
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

    /** These methods manually implement "overloading" among boxed primitives.
     *  The compiler is capable of inserting (but does not presently) the specific
     *  equals method based on the statically known types of the boxes.
     */

    public static boolean equalsCharacterCharacter(Character a, Character b) {
        if (a == null || b == null) return (Object)a == (Object)b;
        else return a.charValue() == b.charValue();
    }
    public static boolean equalsCharacterByte(Character a, Byte b) {
        if (a == null || b == null) return (Object)a == (Object)b;
        else return a.charValue() == b.byteValue();
    }
    public static boolean equalsCharacterShort(Character a, Short b) {
        if (a == null || b == null) return (Object)a == (Object)b;
        else return a.charValue() == b.shortValue();
    }
    public static boolean equalsCharacterInteger(Character a, Integer b) {
        if (a == null || b == null) return (Object)a == (Object)b;
        else return a.charValue() == b.intValue();
    }
    public static boolean equalsCharacterLong(Character a, Long b) {
        if (a == null || b == null) return (Object)a == (Object)b;
        else return a.charValue() == b.longValue();
    }
    public static boolean equalsCharacterFloat(Character a, Float b) {
        if (a == null || b == null) return (Object)a == (Object)b;
        else return a.charValue() == b.floatValue();
    }
    public static boolean equalsCharacterDouble(Character a, Double b) {
        if (a == null || b == null) return (Object)a == (Object)b;
        else return a.charValue() == b.doubleValue();
    }
    public static boolean equalsByteCharacter(Byte a, Character b) {
        if (a == null || b == null) return (Object)a == (Object)b;
        else return a.byteValue() == b.charValue();
    }
    public static boolean equalsByteByte(Byte a, Byte b) {
        if (a == null || b == null) return (Object)a == (Object)b;
        else return a.byteValue() == b.byteValue();
    }
    public static boolean equalsByteShort(Byte a, Short b) {
        if (a == null || b == null) return (Object)a == (Object)b;
        else return a.byteValue() == b.shortValue();
    }
    public static boolean equalsByteInteger(Byte a, Integer b) {
        if (a == null || b == null) return (Object)a == (Object)b;
        else return a.byteValue() == b.intValue();
    }
    public static boolean equalsByteLong(Byte a, Long b) {
        if (a == null || b == null) return (Object)a == (Object)b;
        else return a.byteValue() == b.longValue();
    }
    public static boolean equalsByteFloat(Byte a, Float b) {
        if (a == null || b == null) return (Object)a == (Object)b;
        else return a.byteValue() == b.floatValue();
    }
    public static boolean equalsByteDouble(Byte a, Double b) {
        if (a == null || b == null) return (Object)a == (Object)b;
        else return a.byteValue() == b.doubleValue();
    }
    public static boolean equalsShortCharacter(Short a, Character b) {
        if (a == null || b == null) return (Object)a == (Object)b;
        else return a.shortValue() == b.charValue();
    }
    public static boolean equalsShortByte(Short a, Byte b) {
        if (a == null || b == null) return (Object)a == (Object)b;
        else return a.shortValue() == b.byteValue();
    }
    public static boolean equalsShortShort(Short a, Short b) {
        if (a == null || b == null) return (Object)a == (Object)b;
        else return a.shortValue() == b.shortValue();
    }
    public static boolean equalsShortInteger(Short a, Integer b) {
        if (a == null || b == null) return (Object)a == (Object)b;
        else return a.shortValue() == b.intValue();
    }
    public static boolean equalsShortLong(Short a, Long b) {
        if (a == null || b == null) return (Object)a == (Object)b;
        else return a.shortValue() == b.longValue();
    }
    public static boolean equalsShortFloat(Short a, Float b) {
        if (a == null || b == null) return (Object)a == (Object)b;
        else return a.shortValue() == b.floatValue();
    }
    public static boolean equalsShortDouble(Short a, Double b) {
        if (a == null || b == null) return (Object)a == (Object)b;
        else return a.shortValue() == b.doubleValue();
    }
    public static boolean equalsIntegerCharacter(Integer a, Character b) {
        if (a == null || b == null) return (Object)a == (Object)b;
        else return a.intValue() == b.charValue();
    }
    public static boolean equalsIntegerByte(Integer a, Byte b) {
        if (a == null || b == null) return (Object)a == (Object)b;
        else return a.intValue() == b.byteValue();
    }
    public static boolean equalsIntegerShort(Integer a, Short b) {
        if (a == null || b == null) return (Object)a == (Object)b;
        else return a.intValue() == b.shortValue();
    }
    public static boolean equalsIntegerInteger(Integer a, Integer b) {
        if (a == null || b == null) return (Object)a == (Object)b;
        else return a.intValue() == b.intValue();
    }
    public static boolean equalsIntegerLong(Integer a, Long b) {
        if (a == null || b == null) return (Object)a == (Object)b;
        else return a.intValue() == b.longValue();
    }
    public static boolean equalsIntegerFloat(Integer a, Float b) {
        if (a == null || b == null) return (Object)a == (Object)b;
        else return a.intValue() == b.floatValue();
    }
    public static boolean equalsIntegerDouble(Integer a, Double b) {
        if (a == null || b == null) return (Object)a == (Object)b;
        else return a.intValue() == b.doubleValue();
    }
    public static boolean equalsLongCharacter(Long a, Character b) {
        if (a == null || b == null) return (Object)a == (Object)b;
        else return a.longValue() == b.charValue();
    }
    public static boolean equalsLongByte(Long a, Byte b) {
        if (a == null || b == null) return (Object)a == (Object)b;
        else return a.longValue() == b.byteValue();
    }
    public static boolean equalsLongShort(Long a, Short b) {
        if (a == null || b == null) return (Object)a == (Object)b;
        else return a.longValue() == b.shortValue();
    }
    public static boolean equalsLongInteger(Long a, Integer b) {
        if (a == null || b == null) return (Object)a == (Object)b;
        else return a.longValue() == b.intValue();
    }
    public static boolean equalsLongLong(Long a, Long b) {
        if (a == null || b == null) return (Object)a == (Object)b;
        else return a.longValue() == b.longValue();
    }
    public static boolean equalsLongFloat(Long a, Float b) {
        if (a == null || b == null) return (Object)a == (Object)b;
        else return a.longValue() == b.floatValue();
    }
    public static boolean equalsLongDouble(Long a, Double b) {
        if (a == null || b == null) return (Object)a == (Object)b;
        else return a.longValue() == b.doubleValue();
    }
    public static boolean equalsFloatCharacter(Float a, Character b) {
        if (a == null || b == null) return (Object)a == (Object)b;
        else return a.floatValue() == b.charValue();
    }
    public static boolean equalsFloatByte(Float a, Byte b) {
        if (a == null || b == null) return (Object)a == (Object)b;
        else return a.floatValue() == b.byteValue();
    }
    public static boolean equalsFloatShort(Float a, Short b) {
        if (a == null || b == null) return (Object)a == (Object)b;
        else return a.floatValue() == b.shortValue();
    }
    public static boolean equalsFloatInteger(Float a, Integer b) {
        if (a == null || b == null) return (Object)a == (Object)b;
        else return a.floatValue() == b.intValue();
    }
    public static boolean equalsFloatLong(Float a, Long b) {
        if (a == null || b == null) return (Object)a == (Object)b;
        else return a.floatValue() == b.longValue();
    }
    public static boolean equalsFloatFloat(Float a, Float b) {
        if (a == null || b == null) return (Object)a == (Object)b;
        else return a.floatValue() == b.floatValue();
    }
    public static boolean equalsFloatDouble(Float a, Double b) {
        if (a == null || b == null) return (Object)a == (Object)b;
        else return a.floatValue() == b.doubleValue();
    }
    public static boolean equalsDoubleCharacter(Double a, Character b) {
        if (a == null || b == null) return (Object)a == (Object)b;
        else return a.doubleValue() == b.charValue();
    }
    public static boolean equalsDoubleByte(Double a, Byte b) {
        if (a == null || b == null) return (Object)a == (Object)b;
        else return a.doubleValue() == b.byteValue();
    }
    public static boolean equalsDoubleShort(Double a, Short b) {
        if (a == null || b == null) return (Object)a == (Object)b;
        else return a.doubleValue() == b.shortValue();
    }
    public static boolean equalsDoubleInteger(Double a, Integer b) {
        if (a == null || b == null) return (Object)a == (Object)b;
        else return a.doubleValue() == b.intValue();
    }
    public static boolean equalsDoubleLong(Double a, Long b) {
        if (a == null || b == null) return (Object)a == (Object)b;
        else return a.doubleValue() == b.longValue();
    }
    public static boolean equalsDoubleFloat(Double a, Float b) {
        if (a == null || b == null) return (Object)a == (Object)b;
        else return a.doubleValue() == b.floatValue();
    }
    public static boolean equalsDoubleDouble(Double a, Double b) {
        if (a == null || b == null) return (Object)a == (Object)b;
        else return a.doubleValue() == b.doubleValue();
    }

    /** The current equals method, whose logic is under review. **/

    public static boolean equals(Object a, Object b) {
        if ((a instanceof Number || a instanceof Character) && (b instanceof Number || b instanceof Character)) {
          if (a.getClass() != b.getClass()) {
            logInternal("[ BOXED ] Comparing: ", a, b, whereAreWe());
          }
        }

        if (a == null || b == null)
            return a == b;
        if (a.equals(b))
            return true;
        if (a instanceof Number || a instanceof Character || b instanceof Number || b instanceof Character) {
            int acode = typeCode(a);
            int bcode = typeCode(b);
            int maxcode = (acode < bcode) ? bcode : acode;
            boolean res = false;
            if (maxcode <= INT) {
                int aa = (acode == CHAR) ? ((Character) a).charValue() : ((Number) a).intValue();
                int bb = (bcode == CHAR) ? ((Character) b).charValue() : ((Number) b).intValue();
                res = (aa == bb);
            }
            if (maxcode <= LONG) {
                long aa = (acode == CHAR) ? ((Character) a).charValue() : ((Number) a).longValue();
                long bb = (bcode == CHAR) ? ((Character) b).charValue() : ((Number) b).longValue();
                res = (aa == bb);
            }
            if (maxcode <= FLOAT) {
                float aa = (acode == CHAR) ? ((Character) a).charValue() : ((Number) a).floatValue();
                float bb = (bcode == CHAR) ? ((Character) b).charValue() : ((Number) b).floatValue();
                res = (aa == bb);
            }
            if (maxcode <= DOUBLE) {
                double aa = (acode == CHAR) ? ((Character) a).charValue() : ((Number) a).doubleValue();
                double bb = (bcode == CHAR) ? ((Character) b).charValue() : ((Number) b).doubleValue();
                res = (aa == bb);
            }

            if (res || b.equals(a)) {
              String msg;
              if (res) msg = "[ BOXED ] Overriding equals between different types: ";
              else msg = "[ BOXED ] Overriding equals because b.equals(a): ";
              logInternal(msg, a, b, whereAreWe());
              return true;
            }
            return false;
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
