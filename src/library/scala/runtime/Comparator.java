package scala.runtime;

public class Comparator {

    /* A rich implementation of the equals method that overrides the default equals because Java's boxed primitives are
     * utterly broken. This equals is inserted instead of a normal equals by the Scala compiler (in the icode phase,
     * method genEqEqPrimitive) only when either side of the comparison is a subclass of AnyVal, of java.lang.Number, of
     * java.lang.Character or is exactly Any or AnyRef. */
    public static boolean equals(Object a, Object b) {
        if (a == null)
            return b == null;
        else if (a.equals(b))
            return true;
        else if (a == b)
            return true;
        else if ((a instanceof Byte || a instanceof Short || a instanceof Integer) && b instanceof Number)
            return ((Number)a).intValue() == ((Number)b).intValue();
        else if (a instanceof Number && (b instanceof Byte || b instanceof Short || b instanceof Integer))
            return ((Number)a).intValue() == ((Number)b).intValue();
        else if (a instanceof Long && b instanceof Number)
            return ((Long)a).longValue() == ((Number)b).longValue();
        else if (a instanceof Number && b instanceof Long)
            return ((Number)a).longValue() == ((Long)b).longValue();
        else if (a instanceof Float && b instanceof Number)
            return ((Float)a).floatValue() == ((Number)b).floatValue();
        else if (a instanceof Number && b instanceof Float)
            return ((Number)a).floatValue() == ((Float)b).floatValue();
        else if (a instanceof Number && b instanceof Number)
            return ((Number)a).doubleValue() == ((Number)b).doubleValue();
        else if (a instanceof Number && b instanceof Character)
            return ((Number)a).intValue() == ((Character)b).charValue();
        else if (a instanceof Character && b instanceof Number)
            return ((Character)a).charValue() == ((Number)b).intValue();
        else if (a instanceof Character && b instanceof Character)
            return ((Character)a).charValue() == ((Character)b).charValue();
        else
            return false;
    }

}
