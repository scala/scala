/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala;

public abstract class Boolean extends AnyVal {

    public final boolean value;

    public Boolean(boolean value) {
        this.value = value;
    }

    public boolean equals(java.lang.Object other) {
        return other instanceof Boolean && value == ((Boolean)other).value;
    }
    public int hashCode() {
        int  bits = value ? 1231 : 1237;
        return bits;
    }
    public String toString() {
        return String.valueOf(value);
    }

    /** @meta method (scala.Any)scala.Boolean; */
    public boolean $eq$eq  (java.lang.Object other) { return  equals(other); }
    /** @meta method (scala.Any)scala.Boolean; */
    public boolean $bang$eq(java.lang.Object other) { return !equals(other); }

    /** @meta method []scala.Boolean; */
    public boolean $bang      (            ) { return !value        ; }



    public String  $plus      (String  that) { return  value +  that; }

    public boolean $eq$eq     (boolean that) { return  value == that; }
    public boolean $bang$eq   (boolean that) { return  value != that; }
    public boolean $bar$bar   (boolean that) { return  value || that; }
    public boolean $amp$amp   (boolean that) { return  value && that; }
    public boolean $bar       (boolean that) { return  value |  that; }
    public boolean $amp       (boolean that) { return  value &  that; }
    public boolean $up        (boolean that) { return  value ^  that; }

}
