/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.util;

/**
 * This interface provides methods to issue information, warning and
 * error messages.
 */
public interface Reporter {

    //########################################################################
    // Public Methods - Flags

    /** Are information messages issued? */
    public boolean verbose();
    /** Are warnings issued? */
    public boolean nowarn();
    /** Is a prompt displayed after errors and warnings? */
    public boolean prompt();

    /** Sets whether information messages are issued. */
    public void verbose(boolean verbose);
    /** Sets whether warnings are issued. */
    public void nowarn(boolean nowarn);
    /** Sets whether a prompt is displayed after errors and warnings. */
    public void prompt(boolean prompt);

    //########################################################################
    // Public Methods - Count

    /** Returns the number of warnings issued. */
    public int warnings();

    /** Returns the number of errors issued. */
    public int errors();

    /** Resets all counters. */
    public void resetCounters();

    //########################################################################
    // Public Methods - Report

    /**
     * Issues an information. The position may be null. If force is
     * true, the message is displayed even in non-verbose mode.
     */
    public void info(Position position, String message, boolean force);

    /** Issues a warning. The position may be null. */
    public void warning(Position position, String message);

    /** Issues an error. The position may be null. */
    public void error(Position position, String message);

    //########################################################################
}
