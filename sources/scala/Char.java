/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $OldId: Char.java,v 1.11 2002/05/15 15:03:38 roeckl Exp $
// $Id$

package scala;


public abstract class Char extends Int {

    public String toString() { return String.valueOf((char)asInt()); }

    public int hashCode() { return asInt(); }

    /** @meta method scala.Boolean;
     */
    public boolean isDigit() { return Character.isDigit((char)asInt()); }

    /** @meta method scala.Boolean;
     */
    public boolean isLetter() { return Character.isLetter((char)asInt()); }

    /** @meta method scala.Boolean;
     */
    public boolean isLetterOrDigit() { return Character.isLetterOrDigit((char)asInt()); }

    /** @meta method scala.Boolean;
     */
    public boolean isWhitespace() { return Character.isWhitespace((char)asInt()); }

    public boolean equals(java.lang.Object obj) {
        return obj instanceof Char && $eq$eq(((Char)obj).asChar());
    }
}
