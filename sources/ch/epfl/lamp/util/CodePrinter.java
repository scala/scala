/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package ch.epfl.lamp.util;

import java.io.Writer;
import java.io.StringWriter;
import java.io.IOException;

/**
 * This class provides methods for printing code. It has support for
 * indentation and spacing.
 */
public class CodePrinter {

    //########################################################################
    // Public Constants

    /** The default indentation string */
    public static final String STEP = "    ";

    /** The line separator */
    public static final String LINE = System.getProperty("line.separator");

    //########################################################################
    // Private Fields

    /** The underlying writer */
    private final Writer writer;

    /** The indentation string */
    private String step;

    /** The indentation level */
    private int level;

    /** Do we need to align? */
    private boolean align;

    /** Do we need to print a white space? */
    private boolean space;

    /** Is there already a empty line? */
    private boolean line;

    //########################################################################
    // Public Constructors

    /** Creates a new instance. */
    public CodePrinter() {
        this(new StringWriter());
    }

    /** Creates a new instance. */
    public CodePrinter(String step) {
        this(new StringWriter(), step);
    }

    /** Creates a new instance. */
    public CodePrinter(Writer writer) {
        this(writer, STEP);
    }

    /** Creates a new instance. */
    public CodePrinter(Writer writer, String step) {
        this.writer = writer;
        this.step = step;
        this.level = 0;
        this.align = false;
        this.space = false;
        this.line = false;
    }

    //########################################################################
    // Public Methods - Getting & Setting

    /** Returns the underlying writer. */
    public Writer getWriter() {
        return writer;
    }

    /** Returns the indentation level. */
    public int getIndentLevel() {
        return level;
    }

    /** Returns the indentation width. */
    public int getIndentWidth() {
        return step == null ? -1 : step.length();
    }

    /** Returns the indentation string. */
    public String getIndentString() {
        return step;
    }

    /** Sets the indentation level. */
    public CodePrinter setIndentLevel(int level) {
        this.level = level;
        return this;
    }

    /** Sets the indentation width. */
    public CodePrinter setIndentWidth(int width) {
        StringBuffer buffer = new StringBuffer(width);
        for (int i = 0; i < width; i++) buffer.append(' ');
        return setIndentString(buffer.toString());
    }

    /** Sets the indentation string. */
    public CodePrinter setIndentString(String step) {
        this.step = step;
        return this;
    }

    //########################################################################
    // Public Methods - Formating

    /** Increases the indentation level by one. */
    public CodePrinter indent() {
        level++;
        return this;
    }

    /** Decreases the indentation level by one. */
    public CodePrinter undent() {
        level--;
        return this;
    }

    /** Inserts a new line. */
    public CodePrinter line() {
        if (step == null) return space();
        if (line) return this;
        write(LINE);
        line = align;
        align = true;
        space = false;
        return this;
    }

    /** Inserts a white space. */
    public CodePrinter space() {
        space = !align;
        return this;
    }

    //########################################################################
    // Public Methods - Printing

    /** Prints a new line. */
    public CodePrinter println() {
        return line();
    }

    /** Prints the boolean value followed by a new line. */
    public CodePrinter println(boolean value) {
        return print(value).line();
    }

    /** Prints the byte value followed by a new line. */
    public CodePrinter println(byte value) {
        return print(value).line();
    }

    /** Prints the short value followed by a new line. */
    public CodePrinter println(short value) {
        return print(value).line();
    }

    /** Prints the char value followed by a new line. */
    public CodePrinter println(char value) {
        return print(value).line();
    }

    /** Prints the int value followed by a new line. */
    public CodePrinter println(int value) {
        return print(value).line();
    }

    /** Prints the long value followed by a new line. */
    public CodePrinter println(long value) {
        return print(value).line();
    }

    /** Prints the float value followed by a new line. */
    public CodePrinter println(float value) {
        return print(value).line();
    }

    /** Prints the double value followed by a new line. */
    public CodePrinter println(double value) {
        return print(value).line();
    }

    /** Prints the string followed by a new line. */
    public CodePrinter println(String value) {
        return print(value).line();
    }

    /** Prints the boolean value. */
    public CodePrinter print(boolean value) {
        return print(String.valueOf(value));
    }

    /** Prints the byte value. */
    public CodePrinter print(byte value) {
        return print(String.valueOf(value));
    }

    /** Prints the short value. */
    public CodePrinter print(short value) {
        return print(String.valueOf(value));
    }

    /** Prints the char value. */
    public CodePrinter print(char value) {
        return print(String.valueOf(value));
    }

    /** Prints the int value. */
    public CodePrinter print(int value) {
        return print(String.valueOf(value));
    }

    /** Prints the long value. */
    public CodePrinter print(long value) {
        return print(String.valueOf(value));
    }

    /** Prints the float value. */
    public CodePrinter print(float value) {
        return print(String.valueOf(value));
    }

    /** Prints the long value. */
    public CodePrinter print(double value) {
        return print(String.valueOf(value));
    }

    /** Prints the string. */
    public CodePrinter print(String value) {
        if (align) for (int i = 0; i < level; i++) write(step);
        if (space) write(" ");
        write(value);
        align = false;
        space = false;
        line = false;
        return this;
    }

    //########################################################################
    // Public Methods - Converting

    /** Returns the string representation of this printer. */
    public String toString() {
        return writer.toString();
    }

    //########################################################################
    // Private Methods

    private void write(String string) {
        try {
            writer.write(string);
        } catch (IOException exception) {
            throw new Error(exception);
        }
    }

    //########################################################################
}
