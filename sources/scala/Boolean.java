/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $OldId: Boolean.java,v 1.19 2002/09/12 21:23:08 paltherr Exp $
// $Id$

package scala;

/** @meta class extends scala.AnyVal;
 */
public abstract class Boolean extends AnyVal {

    public abstract boolean asBoolean();

    public boolean $amp$amp(boolean that) {
        return asBoolean() && that;
    }

    public boolean $bar$bar(boolean that) {
        return asBoolean() || that;
    }

    public boolean $amp(boolean that) {
        return asBoolean() & that;
    }

    public boolean $bar(boolean that) {
        return asBoolean() | that;
    }

    public boolean $up(boolean that) {
        return asBoolean() ^ that;
    }

    /** @meta method []scala.Boolean;
     */
    public boolean $bang() {
         return !asBoolean();
    }

    public boolean $eq$eq(boolean that) {
        return asBoolean() == that;
    }

    public boolean $bang$eq(boolean that) {
        return asBoolean() != that;
    }

    public String toString() {
        return String.valueOf(asBoolean());
    }

    public int hashCode() {
        return asBoolean() ? 1231 : 1237;
    }

    public boolean equals(java.lang.Object obj) {
        return obj instanceof Boolean && $eq$eq(((Boolean)obj).asBoolean());
    }

    /** @meta method (scala.Any)scala.Boolean;
     */
    public boolean $eq$eq(java.lang.Object obj) { return equals(obj); }

    /** @meta method (scala.Any)scala.Boolean;
     */
    public boolean $bang$eq(java.lang.Object obj) { return !equals(obj); }
}
