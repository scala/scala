/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package ch.epfl.lamp.util;

import java.io.File;
import java.io.InputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;

/**
 * This class represents a single source file.
 */
public class SourceFile {

    //########################################################################
    // Public Constants

    /** Constants used for source parsing */
    public static final byte LF = 0x0A;
    public static final byte FF = 0x0C;
    public static final byte CR = 0x0D;
    public static final byte SU = 0x1A;

    //########################################################################
    // Private Fields

    /** The name of this source file */
    private final String name;

    /** The content of source this file */
    private final byte[] bytes;

    /** The encoding of this source file or null if unspecified */
    private String encoding;

    /** The position of the last line returned by getLine */
    private int lineNumber = 0;
    private int lineStart  = 0;
    private int lineLength = 0;
    private int nextIndex  = 0;

    //########################################################################
    // Public Constructors

    /** Initializes a new instance. */
    public SourceFile(String name) throws IOException {
        this(new File(name));
    }

    /** Initializes a new instance. */
    public SourceFile(File name) throws IOException {
        this(name.toString(), read(name));
    }

    /** Initializes a new instance. */
    public SourceFile(String name, InputStream input) throws IOException {
        this(name, read(name, input));
    }

    /** Initializes a new instance. */
    public SourceFile(String name, byte[] bytes) {
        this.name = name;
        this.bytes = normalize(bytes);
    }

    //########################################################################
    // Public Methods

    /** Returns the name of this source file. */
    public String name() {
        return name;
    }

    /** Returns the content of this source file. */
    public byte[] bytes() {
        return bytes;
    }

    /** Sets the encoding of the file. */
    public void setEncoding(String encoding) {
        this.encoding = encoding;
    }

    /** Returns the content of the given line. */
    public String getLine(int line) {
        int index = lineNumber <= line ? nextIndex : (lineNumber = 0);
        for (; index < bytes.length && lineNumber < line; lineNumber++) {
            lineStart = index;
            for (; index < bytes.length; index++) {
                if (bytes[index] == CR) break;
                if (bytes[index] == LF) break;
                if (bytes[index] == FF) break;
            }
            lineLength = index - lineStart;
            if (index < bytes.length) index++;
            if (index < bytes.length)
                if (bytes[index - 1] == CR && bytes[index] == LF) index++;
        }
        nextIndex = index;
        try {
            return encoding != null ?
                new String(bytes, lineStart, lineLength, encoding) :
                new String(bytes, lineStart, lineLength);
        } catch (UnsupportedEncodingException exception) {
            throw new Error(exception); // !!! use ApplicationError
        }
    }

    /**
     * Returns an instance of Position representing the given line and
     * column of this source file.
     */
    public Position getPosition(int line, int column) {
        return new Position(this, line, column);
    }

    /** Returns the name of this source file. */
    public String toString() {
        return name;
    }

    //########################################################################
    // Private Methods

    /** Reads the file and returns its content as a byte array. */
    private static byte[] read(File file) throws IOException {
        InputStream input = new FileInputStream(file);
        try {
            return read(file.toString(), input);
        } finally {
            input.close();
        }
    }

    /** Reads the InputStream and returns its content as a byte array. */
    private static byte[] read(String name, InputStream input)
        throws IOException
    {
        try {
            byte[] bytes = new byte[input.available() + 1];
            if (input.read(bytes) != bytes.length - 1) throw new IOException();
            bytes[bytes.length - 1] = SU;
            return bytes;
        } catch (IOException exception) {
            throw new IOException("cannot read '" + name + "'");
        }
    }

    /** Ensures that the last byte of the array is SU. */
    private static byte[] normalize(byte[] input) {
        if (input.length > 0 && input[input.length - 1] == SU) return input;
        byte[] bytes = new byte[input.length + 1];
        System.arraycopy(input, 0, bytes, 0, input.length);
        bytes[input.length] = SU;
        return bytes;
    }

    //########################################################################
}
