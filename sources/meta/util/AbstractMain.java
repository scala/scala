/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package meta.util;

/** A base class for Java programs. */
public abstract class AbstractMain {

    //########################################################################
    // Public Functions

    public static String script() {
        StackTraceElement[] stack = new Throwable().getStackTrace();
        return stack[stack.length - 1].getClassName();
    }

    public static Error abort() {
        System.exit(1);
        throw new Error("abort");
    }

    public static Error abort(String error) {
        System.err.println(script() + ": " + error);
        System.exit(1);
        throw new Error();
    }

    public static Error abort(Exception exception) {
        return abort("caught exception " + exception.toString());
    }

    //########################################################################
}
