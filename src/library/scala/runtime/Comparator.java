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
 *  @author  Martin Odersky
 *  @version 1.2 */
public class Comparator {

    private static int CHAR = 0, BYTE = 1, SHORT = 2, INT = 3, LONG = 4, FLOAT = 5, DOUBLE = 6, OTHER = 7;

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

    /** A rich implementation of the equals method that overrides the default
     *  equals because Java's boxed primitives are utterly broken. This equals
     *  is inserted instead of a normal equals by the Scala compiler (in the
     *  ICode phase, method <code>genEqEqPrimitive</code>) only when either
     *  side of the comparison is a subclass of <code>AnyVal</code>, of
     *  <code>java.lang.Number</code>, of <code>java.lang.Character</code> or
     *  is exactly <code>Any</code> or <code>AnyRef</code>.
     */
    public static boolean equals(Object a, Object b) {
        if (a == null)
            return b == null;
        else if (b == null)
            return false;
        else if (a.equals(b))
            return true;
        else {
            int acode = typeCode(a);
            int bcode = typeCode(b);
            int maxcode = (acode < bcode) ? bcode : acode;
            if (maxcode <= INT) {
                int aa = (acode == CHAR) ? ((Character) a).charValue() : ((Number) a).intValue();
                int bb = (bcode == CHAR) ? ((Character) b).charValue() : ((Number) b).intValue();
                return aa == bb;
            } else if (maxcode <= LONG) {
                long aa = (acode == CHAR) ? ((Character) a).charValue() : ((Number) a).longValue();
                long bb = (bcode == CHAR) ? ((Character) b).charValue() : ((Number) b).longValue();
                return aa == bb;
            } else if (maxcode <= FLOAT) {
                float aa = (acode == CHAR) ? ((Character) a).charValue() : ((Number) a).floatValue();
                float bb = (bcode == CHAR) ? ((Character) b).charValue() : ((Number) b).floatValue();
                return aa == bb;
            } else if (maxcode <= DOUBLE) {
                double aa = (acode == CHAR) ? ((Character) a).charValue() : ((Number) a).doubleValue();
                double bb = (bcode == CHAR) ? ((Character) b).charValue() : ((Number) b).doubleValue();
                return aa == bb;
            } else if (acode != OTHER) {
                return b.equals(a);
            } else {
                return a == b;
            }
        }
    }
}
