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
 *  @version 1.1
 */
public class Comparator {

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
        else if (a.equals(b))
            return true;
        else if (a == b)
            return true;

        final Number aNum = a instanceof Number ? (Number)a : null;
        final Character aChar = a instanceof Character ? (Character)a : null;
        final Number bNum = b instanceof Number ? (Number)b : null;
        final Character bChar = b instanceof Character ? (Character)b : null;

        if ((aNum != null || aChar != null) && (bNum != null || bChar != null)) {
            if (a instanceof Double || b instanceof Double) {
                double aa = (aNum != null) ? aNum.doubleValue() : (double)aChar.charValue();
                double bb = (bNum != null) ? bNum.doubleValue() : (double)bChar.charValue();
                return aa == bb;
            }
            else if (a instanceof Float || b instanceof Float) {
                float aa = (aNum != null) ? aNum.floatValue() : (float)aChar.charValue();
                float bb = (bNum != null) ? bNum.floatValue() : (float)bChar.charValue();
                return aa == bb;
            }
            else if (a instanceof Long || b instanceof Long) {
                long aa = (aNum != null) ? aNum.longValue() : (long)aChar.charValue();
                long bb = (bNum != null) ? bNum.longValue() : (long)bChar.charValue();
                return aa == bb;
            }
            else {
                int aa = (aNum != null) ? aNum.intValue() : aChar.charValue();
                int bb = (bNum != null) ? bNum.intValue() : bChar.charValue();
                return aa == bb;
            }
        }
        else
            return false;
    }

}
