/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.util.debug;

/**
 * This class implements a debugger that appends any object. It
 * appends the class name of the object and either the string returned
 * by its method "toString" if it overridden or its identity hash code
 * otherwise.
 */
public class ObjectDebugger implements Debugger {

    //########################################################################
    // Public Constants

    /** The unique instance of this class. */
    public static final ObjectDebugger object = new ObjectDebugger();

    //########################################################################
    // Protected Constructors

    /** Initializes this instance. */
    protected ObjectDebugger() {}

    //########################################################################
    // Public Methods

    public boolean canAppend(Object object) {
        return true;
    }

    public void append(StringBuffer buffer, Object object) {
        buffer.append(Debug.getClassNameOf(object));
        Class owner = null;
        if (Debug.overridesToString(object)) {
            buffer.append('(');
            buffer.append(object);
            buffer.append(')');
        } else {
            String code = Integer.toHexString(System.identityHashCode(object));
            buffer.append('@');
            for (int i = code.length(); i < 8; i++) buffer.append('0');
            buffer.append(code);
        }
    }

    //########################################################################
}
