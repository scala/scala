package scala.runtime;

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

    public static Boolean boxToBoolean(boolean b) {
        return b ? Boolean.TRUE : Boolean.FALSE;
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

    public static Float boxToFloat(float f) {
        return new Float(f);
    }

    public static Double boxToDouble(double d) {
        return new Double(d);
    }

    public static boolean unboxToBoolean(Object b) {
        return b == null ? false : ((Boolean)b).booleanValue();
    }

    public static char unboxToChar(Object c) {
        if (c == null)
            return 0;
        else if (c instanceof Character)
            return ((Character)c).charValue();
        else
            return ((char)((Number)c).intValue());
    }

    public static byte unboxToByte(Object b) {
        if (b == null)
            return 0;
        else if (b instanceof Number)
            return ((Number)b).byteValue();
        else
            return ((byte)((Character)b).charValue());
    }

    public static short unboxToShort(Object s) {
        if (s == null)
            return 0;
        else if (s instanceof Number)
            return ((Number)s).shortValue();
        else
            return ((short)((Character)s).charValue());
    }

    public static int unboxToInt(Object i) {
        if (i == null)
            return 0;
        else if (i instanceof Number)
            return ((Number)i).intValue();
        else
            return ((int)((Character)i).charValue());
    }

    public static long unboxToLong(Object l) {
        if (l == null)
            return 0;
        else if (l instanceof Number)
            return ((Number)l).longValue();
        else
            return ((long)((Character)l).charValue());
    }

    public static float unboxToFloat(Object f) {
        if (f == null)
            return 0.0f;
        else if (f instanceof Number)
            return ((Number)f).floatValue();
        else
            return ((float)((Character)f).charValue());
    }

    public static double unboxToDouble(Object d) {
        if (d == null)
            return 0.0;
        else if (d instanceof Number)
            return ((Number)d).doubleValue();
        else
            return ((double)((Character)d).charValue());
    }

}
