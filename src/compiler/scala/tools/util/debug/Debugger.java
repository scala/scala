/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.util.debug;

/**
 * This interface defines methods used by the class Debug to turn
 * objects into strings.
 */
public interface Debugger {

    //########################################################################
    // Public Methods

    /**
     * Returns "true" if the specified object may be passed as an
     * argument to the method "append".
     */
    public boolean canAppend(Object object);

    /**
     * Appends the object to the string buffer. This method must be
     * invoked only with objects for which the method "canAppend"
     * returns "true".
     */
    public void append(StringBuffer buffer, Object object);

    //########################################################################
}
