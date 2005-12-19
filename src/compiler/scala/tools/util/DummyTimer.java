/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.util;

import java.util.ArrayList;

/** This class implements a timer that does nothing. */
public class DummyTimer implements Timer {

    //########################################################################
    // Public Constants

    /** The unique instance of this class. */
    public static final DummyTimer object = new DummyTimer();

    //########################################################################
    // Private Constructors

    /** Initializes this instance. */
    private DummyTimer() {}

    //########################################################################
    // Public Methods

    /** Starts a new timer. */
    public void start() {
    }

    /** Ends the current timer. */
    public void stop(String message) {
    }

    /** Drops the current timer. */
    public void drop() {
    }

    //########################################################################
}
