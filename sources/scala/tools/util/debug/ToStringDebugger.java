/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.util.debug;

/**
 * This class implements a debugger that appends objects that are
 * instances of a specified class (or of one of its subclass) by
 * simply appending the string returned by their method "toString".
 */
public class ToStringDebugger implements Debugger {

    //########################################################################
    // Private Fields

    /** The class whose instances can be appended */
    private final Class clasz;

    //########################################################################
    // Public Constructors

    /** Initializes this instance. */
    public ToStringDebugger(Class clasz) {
        this.clasz = clasz;
    }

    //########################################################################
    // Public Methods

    public boolean canAppend(Object object) {
        return clasz.isInstance(object);
    }

    public void append(StringBuffer buffer, Object object) {
        buffer.append(object.toString());
    }

    //########################################################################
}
