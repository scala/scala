/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2009, LAMP/EPFL             **
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

    /** A rich implementation of the equals method that overrides the default
     *  equals because Java's boxed primitives are utterly broken. This equals
     *  is inserted instead of a normal equals by the Scala compiler (in the
     *  ICode phase, method <code>genEqEqPrimitive</code>) only when either
     *  side of the comparison is a subclass of <code>AnyVal</code>, of
     *  <code>java.lang.Number</code>, of <code>java.lang.Character</code> or
     *  is exactly <code>Any</code> or <code>AnyRef</code>, but when both sides
     *  have different types. */
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

}
