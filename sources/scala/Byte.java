/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $OldId: Byte.java,v 1.9 2002/04/15 16:21:39 paltherr Exp $
// $Id$

package scala;


public abstract class Byte extends Short {
    public String toString() { return String.valueOf(asByte()); }
    public int hashCode() { return asInt(); }
    public boolean equals(java.lang.Object obj) {
        return obj instanceof Byte && $eq$eq(((Byte)obj).asByte());
    }
}
