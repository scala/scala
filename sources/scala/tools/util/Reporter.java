/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.util;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.HashSet;

import scala.tools.util.debug.Debug;

public class Reporter {

    //########################################################################
    // Private Fields

    /** The reader to ask for failures on demand */
    private final BufferedReader reader;
    /** The writer to print messages */
    private final PrintWriter writer;

    /** Log of error positions (used to avoid printing errors twice) */
    private final HashSet positions;

    /** Number of errors issued totally */
    private int errors;
    /** Number of warning issued totally */
    private int warnings;

    //########################################################################
    // Public Fields

    /** Whether warnings should be issued */
    public boolean nowarn;
    /** Whether information messages should be issued */
    public boolean verbose;
    /** Whether a prompt should be displayed after errors and warnings */
    public boolean prompt;
    /** Whether a short file name should be displayed before errors */
    public boolean shortname;

    //########################################################################
    // Public Constructors

    /** Initializes a new instance. */
    public Reporter() {
        this(
            new BufferedReader(new InputStreamReader(System.in)),
            new PrintWriter(System.err, true));
    }

    /** Initializes a new instance. */
    public Reporter(BufferedReader reader, PrintWriter writer) {
        this.reader = reader;
        this.writer = writer;
        this.positions = new HashSet();
        this.prompt = false;
        this.nowarn = false;
        this.verbose = false;
        this.errors = 0;
    }

    //########################################################################
    // Public Methods - Count

    /** Returns the number of errors issued totally */
    public int errors() {
        return errors;
    }

    /** Returns the number of warnings issued totally */
    public int warnings() {
        return warnings;
    }

    /** Returns the number of errors issued totally as a string */
    public String getErrorCountString() {
        return getCountString(errors, "error");
    }

    /** Returns the number of warnings issued totally as a string */
    public String getWarningCountString() {
        return getCountString(warnings, "warning");
    }

    /** Returns a string meaning "n elements". */
    public String getCountString(int n, String elements) {
        switch (n) {
        case 0: return "no " + elements + "s";
        case 1: return "one " + elements;
        case 2: return "two " + elements + "s";
        case 3: return "three " + elements + "s";
        case 4: return "four " + elements + "s";
        default: return n + " " + elements + "s";
        }
    }

    /** Resets all counters */
    public void resetCounters() {
        errors = 0;
        warnings = 0;
    }

    //########################################################################
    // Public Methods - Report

    /** Issues a message */
    public void report(String message) {
        printMessage(message);
    }

    /** Issues a message */
    public void inform(String message) {
        if (verbose) printMessage(message);
    }

    /** Issues an error */
    public void error(Position position, String message) {
        boolean hidden = testAndLog(position);
        if (!hidden || prompt) printError(position, message);
        if (!hidden) errors++;
        if (prompt) failOnDemand();
    }

    /** Issues a warning */
    public void warning(Position position, String message) {
        boolean hidden = testAndLog(position);
        if (nowarn) return;
        if (!hidden || prompt) printWarning(position, message);
        if (!hidden) warnings++;
        if (prompt) failOnDemand();
    }

    //########################################################################
    // Public Methods - Print

    /** Prints the message. */
    public void printMessage(String message) {
        writer.println(message);
    }

    /** Prints the message with the given position indication. */
    public void printMessage(Position position, String message) {
        if (position != null) {
            message = " " + message;
            if (position.getLineNumber() != 0)
                message = position.getLineNumber() + ":" + message;
            if (shortname)
                message = position.getName() + ":" + message;
            else
                message = position.getPath() + ":" + message;
        }
        printMessage(message);
        printSourceLine(position);
    }

    /** Prints the error message. */
    public void printError(Position position, String message) {
        if (position == null) message = "error: " + message;
        printMessage(position, message);
    }

    /** Prints the warning message. */
    public void printWarning(Position position, String message) {
        message = "warning: " + message;
        printMessage(position, message);
    }

    /** Prints the number of errors and warnings if their are non-zero. */
    public void printSummary() {
        if (errors() > 0) report(getErrorCountString() + " found");
        if (warnings() > 0) report(getWarningCountString() + " found");
    }

    /** Prints the source line of the given position. */
    public void printSourceLine(Position position) {
        String line = position == null ? null : position.getLineContent();
        if (line == null) return;
        printMessage(line);
        printColumnMarker(position);
    }

    /** Prints the column marker of the given position. */
    public void printColumnMarker(Position position) {
        int column = position == null ? 0 : position.getColumnNumber();
        StringBuffer buffer = new StringBuffer(column);
        for (int i = 1; i < column; i++) buffer.append(' ');
        if (column > 0) buffer.append('^');
        printMessage(buffer.toString());
    }

    //########################################################################
    // Public Methods - Fail on demand

    /** Fails only if requested. */
    public void failOnDemand() {
        failOnDemand("user abort");
    }

    /** Fails only if requested. */
    public void failOnDemand(String message) {
        try {
            while (true) {
                writer.print("r)esume, a)bort: ");
                writer.flush();
                String line = reader.readLine();
                if (line == null) continue; else line = line.toLowerCase();
                if ("abort".startsWith(line))
                    throw new Error(message);
                if ("resume".startsWith(line)) return;
            }
        } catch (IOException e) {
            Debug.abort("input read error");
        }
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
