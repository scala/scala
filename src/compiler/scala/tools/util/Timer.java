/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.util;

/** This interface provides methods to collect timings. */
public interface Timer {

    //########################################################################
    // Public Methods

    /** Starts a new timer. */
    public void start();

    /** Stops the current timer. */
    public void stop(String message);

    /** Drops the current timer. */
    public void drop();

    //########################################################################
}
