/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.util;

import java.io.InputStreamReader;
import java.io.BufferedReader;
import java.io.PrintWriter;
import java.io.IOException;
import scalac.ApplicationError;

public class Reporter {

    //########################################################################
    // Private state

    private final BufferedReader reader;
    private final PrintWriter writer;

    /** Number of errors issued totally */
    private int errors;
    /** Number of warning issued totally */
    private int warnings;

    //########################################################################
    // Reporter constructors

    public Reporter() {
        this(
            new BufferedReader(new InputStreamReader(System.in)),
            new PrintWriter(System.err, true));
    }

    public Reporter(BufferedReader reader, PrintWriter writer) {
        this.reader = reader;
        this.writer = writer;
        this.prompt = false;
        this.nowarn = false;
        this.verbose = false;
        this.errors = 0;
    }

    //########################################################################
    // Reporter state

    /** Whether warnings should be issued */
    public boolean nowarn;
    /** Whether information messages should be issued */
    public boolean verbose;
    /** Whether a prompt should be displayed after errors and warnings */
    public boolean prompt;

    //########################################################################
    // Reporter interface - query

    /** Return the number of errors issued totally */
    public int errors() {
        return errors;
    }

    /** Return the number of warnings issued totally */
    public int warnings() {
        return warnings;
    }

    /** Return the number of errors issued totally as a string */
    public String getErrorCountString() {
        return getCountString(errors, "error");
    }

    /** Return the number of warnings issued totally as a string */
    public String getWarningCountString() {
        return getCountString(warnings, "warning");
    }

    public String getCountString(int count, String what) {
        switch (count) {
        case 0: return "no " + what + "s";
        case 1: return "one " + what;
        case 2: return "two " + what + "s";
        case 3: return "three " + what + "s";
        case 4: return "four " + what + "s";
        default: return count + " " + what + "s";
        }
    }

    //########################################################################
    // Reporter interface - report

    /** Reset all counters */
    public void resetCounters() {
        errors = 0;
        warnings = 0;
    }

    /** Issue a message */
    public void report(String message) {
        writer.println(message);
    }

    /** Issue a message */
    public void inform(String message) {
        if (verbose) report(message);
    }

    /** Issue an error */
    public void error(String message) {
        error(message, false);
    }

    /** Issue an error if it is not hidden */
    public void error(String message, boolean hidden) {
        if (!hidden || prompt) report(message);
        if (!hidden) errors++;
        if (prompt) failOnDemand();
    }

    /** Issue a warning */
    public void warning(String message) {
        warning(message, false);
    }

    /** Issue a warning if it is not hidden */
    public void warning(String message, boolean hidden) {
        if (nowarn) return;
        if (!hidden || prompt) report(message);
        if (!hidden) warnings++;
        if (prompt) failOnDemand();
    }

    public void printSummary() {
        if (errors() > 0) report(getErrorCountString() + " found");
        if (warnings() > 0) report(getWarningCountString() + " found");
    }

    //########################################################################
    // Reporter interface - fail

    /** Fail only if requested */
    public void failOnDemand() {
        failOnDemand("user abort");
    }

    /** Fail only if requested */
    public void failOnDemand(String message) {
        try {
            while (true) {
                writer.print("r)esume, a)bort: ");
                String line = reader.readLine();
                if (line == null) continue; else line = line.toLowerCase();
                if ("abort".startsWith(line))
                    throw new ApplicationError(message);
                if ("resume".startsWith(line)) return;
            }
        } catch (IOException e) {
            throw new ApplicationError("input read error");
        }
    }

    //########################################################################
}
