/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.util;

/**
 * This class implements a timer that uses a Reporter to issue
 * timings.
 */
public class ReporterTimer extends AbstractTimer {

    //########################################################################
    // Private Fields

    /** A reporter to report timing information */
    private final Reporter reporter;

    //########################################################################
    // Public Constructors

    public ReporterTimer(Reporter reporter) {
        this.reporter = reporter;
    }

    //########################################################################
    // Public Methods

    /** Issues a timing information (duration in milliseconds). */
    public void issue(String message, long duration) {
        reporter.inform("[" + message + " in " + duration + "ms]");
    }

    //########################################################################
}
