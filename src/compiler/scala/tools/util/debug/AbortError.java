/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.util.debug;

/**
 * This class implements an error that can be used to abort an
 * application after an internal error.
 */
public class AbortError extends Error {

    //########################################################################
    // Protected Constructors

    /** Initializes this instance. */
    protected AbortError() {
        super();
    }

    /** Initializes this instance with the specified message. */
    protected AbortError(String message) {
        super(message);
    }

    /** Initializes this instance with the specified cause. */
    protected AbortError(Throwable cause) {
        super(cause);
    }

    /**
     * Initializes this instance with the specified message and cause.
     */
    protected AbortError(String message, Throwable cause) {
        super(message, cause);
    }

    //########################################################################
}
