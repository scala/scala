/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.util;

import java.util.ArrayList;

/**
 * This abstract class implements the collection of timings. How the
 * collected timings are issued has to be implemented in subclasses.
 */
public abstract class AbstractTimer implements Timer {

    //########################################################################
    // Private Fields

    /** A stack for maintaining start times */
    private final ArrayList starts = new ArrayList();

    //########################################################################
    // Public Methods

    /** Issues a timing information (duration in milliseconds). */
    public abstract void issue(String message, long duration);

    /** Starts a new timer. */
    public void start() {
        starts.add(new Long(System.currentTimeMillis()));
    }

    /** Ends the current timer. */
    public void stop(String message) {
        long stop = System.currentTimeMillis();
        long start = ((Long)starts.remove(starts.size() - 1)).longValue();
        issue(message, stop - start);
    }

    /** Drops the current timer. */
    public void drop() {
        starts.remove(starts.size() - 1);
    }

    //########################################################################
}
