/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.runtime;

/**
 *  @author  Gilles Dubochet
 *  @version 1.0
 */
public class BoxesUtility {

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
        else
            return new Character(c);
    }

    public static Byte boxToByte(byte b) {
        if (b >= byteLowBound && b <= byteUpBound)
            return byteCache[(int)b - byteLowBound];
        else
            return new Byte(b);
    }

    public static Short boxToShort(short s) {
        if (s >= shortLowBound && s <= shortUpBound)
            return shortCache[(int)s - shortLowBound];
        else
            return new Short(s);
    }

    public static Integer boxToInteger(int i) {
        if (i >= intLowBound && i <= intUpBound)
            return intCache[(int)i - intLowBound];
        else
            return new Integer(i);
    }

    public static Long boxToLong(long l) {
        if (l >= longLowBound && l <= longUpBound)
            return longCache[(int)l - longLowBound];
        else
            return new Long(l);
    }

    public static boolean unboxToBoolean(Object b) {
        return b == null ? false : ((Boolean)b).booleanValue();
    }

    public static char unboxToChar(Object c) {
        if (c == null)
            return 0;
        else
            return ((Character)c).charValue();
    }

    public static byte unboxToByte(Object b) {
        if (b == null)
            return 0;
        else
            return ((Byte)b).byteValue();
    }

    public static short unboxToShort(Object s) {
        if (s == null)
            return 0;
        else
            return ((Short)s).shortValue();
    }

    public static int unboxToInt(Object i) {
        if (i == null)
            return 0;
        else
            return ((Integer)i).intValue();
    }

    public static long unboxToLong(Object l) {
        if (l == null)
            return 0;
        else
            return ((Long)l).longValue();
    }

}
