/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac;

import java.io.PrintStream;
import java.io.PrintWriter;

import scalac.util.Debug;

public class ApplicationError extends Error {

    //########################################################################
    // Private state

    private final Throwable cause; // !!! remove and use API 1.4

    //########################################################################
    // Private interface

    private static String combine(String message, Object object) {
        String string = Debug.show(object);
        return message == null ? string : message + ": " + string;
    }

    //########################################################################
    // ApplicationError constructors

    public ApplicationError() {
        this((String)null, (Throwable)null);
    }

    public ApplicationError(String message) {
        this(message, (Throwable)null);
    }

    public ApplicationError(Object object) {
        this(null, object, null);
    }

    public ApplicationError(Throwable cause) {
        this((String)null, cause);
    }

    public ApplicationError(String message, Object object) {
        this(message, object, null);
    }

    public ApplicationError(String message, Throwable cause) {
        super(message);
        this.cause = cause;
    }

    public ApplicationError(Object object, Throwable cause) {
        this(null, object, cause);
    }

    public ApplicationError(String message, Object object, Throwable cause) {
        this(combine(message, object), cause);
    }

    //########################################################################
    // Throwable interface

    public void printStackTrace(PrintStream printer) {
        super.printStackTrace(printer);
        if (cause != null) {
            printer.print("Caused by: ");
            cause.printStackTrace(printer);
        }
    }

    public void printStackTrace(PrintWriter printer) {
        super.printStackTrace(printer);
        if (cause != null) {
            printer.print("Caused by: ");
            cause.printStackTrace(printer);
        }
    }

    //########################################################################
}
