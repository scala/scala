/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.util.debug;

import java.io.Writer;
import java.io.PrintWriter;

import scala.tools.util.StringBufferWriter;

/**
 * This class implements a debugger that appends instances of
 * Throwable.
 */
public class ThrowableDebugger implements Debugger {

    //########################################################################
    // Public Constants

    /** The unique instance of this class. */
    public static final ThrowableDebugger object = new ThrowableDebugger();

    //########################################################################
    // Protected Constructors

    /** Initializes this instance. */
    protected ThrowableDebugger() {}

    //########################################################################
    // Public Methods

    public boolean canAppend(Object object) {
        return object instanceof Throwable;
    }

    public void append(StringBuffer buffer, Object object) {
        PrintWriter writer = new PrintWriter(new StringBufferWriter(buffer));
        ((Throwable)object).printStackTrace(writer);
        writer.close();
    }

    //########################################################################
}
