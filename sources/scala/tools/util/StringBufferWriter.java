/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.util;

import java.io.Writer;
import java.io.PrintWriter;

/** This class implements a writer that writes to a string buffer. */
public class StringBufferWriter extends Writer {

    //########################################################################
    // Private Fields

    private final StringBuffer buffer;

    //########################################################################
    // Public Constructors

    /** Initializes this instance with the specified string buffer. */
    public StringBufferWriter(StringBuffer buffer) {
        this.buffer = buffer;
    }

    //########################################################################
    // Public Methods

    /** Returns the underlying string buffer. */
    public StringBuffer getStringBuffer() {
        return buffer;
    }

    public void close() {
    }

    public void flush() {
    }

    public void write(int c) {
        buffer.append((char)c);
    }

    public void write(char[] cs) {
        buffer.append(cs);
    }

    public void write(char[] cs, int start, int count) {
        buffer.append(cs, start, count);
    }

    public void write(String string) {
        buffer.append(string);
    }

    public void write(String string, int start, int count) {
        buffer.append(string.substring(start, start + count));
    }

    public String toString() {
        return buffer.toString();
    }

    //########################################################################
}
