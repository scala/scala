/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.util;

import java.util.HashSet;

/**
 * This abstract class implements most aspects of a Reporter, only how
 * things are displayed has to be implemented in subclasses.
 */
public abstract class AbstractReporter implements Reporter {

    //########################################################################
    // Private Fields

    /** Log of error positions (used to avoid printing errors twice) */
    private final HashSet positions;

    /** Whether information messages should be issued */
    private boolean verbose;
    /** Whether warnings should be issued */
    private boolean nowarn;
    /** Whether a prompt should be displayed after errors and warnings */
    private boolean prompt;

    /** Number of warning issued totally */
    private int warnings;
    /** Number of errors issued totally */
    private int errors;

    //########################################################################
    // Public Constructors

    /** Initializes a new instance. */
    public AbstractReporter() {
        this.positions = new HashSet();
        this.verbose = false;
        this.nowarn = false;
        this.prompt = false;
        this.warnings = 0;
        this.errors = 0;
    }

    //########################################################################
    // Public Methods - Display

    /** Displays the information. The position may be null. */
    public abstract void displayInfo(Position position, String message);

    /** Displays the warning. The position may be null. */
    public abstract void displayWarning(Position position, String message);

    /** Displays the error. The position may be null. */
    public abstract void displayError(Position position, String message);

    /** Displays a prompt. */
    public abstract void displayPrompt();

    //########################################################################
    // Public Methods - Flags

    public boolean verbose() {
        return verbose;
    }

    public boolean nowarn() {
        return nowarn;
    }

    public boolean prompt() {
        return prompt;
    }


    public void verbose(boolean verbose) {
        this.verbose = verbose;
    }

    public void nowarn(boolean nowarn) {
        this.nowarn = nowarn;
    }

    public void prompt(boolean prompt) {
        this.prompt = prompt;
    }

    //########################################################################
    // Public Methods - Count

    public int warnings() {
        return warnings;
    }

    public int errors() {
        return errors;
    }

    public void resetCounters() {
        errors = 0;
        warnings = 0;
    }

    //########################################################################
    // Public Methods - Report

    public void info(Position position, String message, boolean force) {
        if (force || verbose) displayInfo(null, message);
    }

    public void warning(Position position, String message) {
        boolean hidden = testAndLog(position);
        if (nowarn) return;
        if (!hidden || prompt) displayWarning(position, message);
        if (!hidden) warnings++;
        if (prompt) displayPrompt();
    }

    public void error(Position position, String message) {
        boolean hidden = testAndLog(position);
        if (!hidden || prompt) displayError(position, message);
        if (!hidden) errors++;
        if (prompt) displayPrompt();
    }

    //########################################################################
    // Private Methods

    /** Logs a position and returns true if it was already logged. */
    private boolean testAndLog(Position position) {
        if (position == null) return false;
        if (position.getColumnNumber() == 0) return false;
        if (positions.contains(position)) return true;
        positions.add(position);
        return false;
    }

    //########################################################################
}
