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

/**
 * This class implements a Reporter that displays messages on a text
 * console.
 */
public class ConsoleReporter extends AbstractReporter {

    //########################################################################
    // Private Fields

    /** The reader to ask for failures on demand */
    private final BufferedReader reader;
    /** The writer to print messages */
    private final PrintWriter writer;

    //########################################################################
    // Public Fields

    /** Whether a short file name should be displayed before errors */
    public boolean shortname;

    //########################################################################
    // Public Constructors

    /** Initializes a new instance. */
    public ConsoleReporter() {
        this(
            new BufferedReader(new InputStreamReader(System.in)),
            new PrintWriter(System.err, true));
    }

    /** Initializes a new instance. */
    public ConsoleReporter(BufferedReader reader, PrintWriter writer) {
        this.reader = reader;
        this.writer = writer;
    }

    //########################################################################
    // Public Methods - Count

    /** Returns the number of errors issued totally as a string */
    public String getErrorCountString() {
        return getCountString(errors(), "error");
    }

    /** Returns the number of warnings issued totally as a string */
    public String getWarningCountString() {
        return getCountString(warnings(), "warning");
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

    /** Prints the warning message. */
    public void printWarning(Position position, String message) {
        message = "warning: " + message;
        printMessage(position, message);
    }

    /** Prints the error message. */
    public void printError(Position position, String message) {
        if (position == null) message = "error: " + message;
        printMessage(position, message);
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

    /** Prints the number of errors and warnings if their are non-zero. */
    public void printSummary() {
        if (warnings() > 0) printMessage(getWarningCountString() + " found");
        if (errors() > 0) printMessage(getErrorCountString() + " found");
    }

    //########################################################################
    // Public Methods - Display

    public void displayInfo(Position position, String message) {
        printMessage(position, message);
    }

    public void displayWarning(Position position, String message) {
        printWarning(position, message);
    }

    public void displayError(Position position, String message) {
        printError(position, message);
    }

    public void displayPrompt() {
        try {
            while (true) {
                writer.print("r)esume, a)bort: ");
                writer.flush();
                String line = reader.readLine();
                if (line == null) continue; else line = line.toLowerCase();
                if ("abort".startsWith(line))
                    throw new Error("user abort");
                if ("resume".startsWith(line)) return;
            }
        } catch (IOException e) {
            throw new Error("input read error");
        }
    }

    //########################################################################
}
