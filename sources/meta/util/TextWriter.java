/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package meta.util;

/** A string generator with support for indentation and spacing. */
public class TextWriter {

    //########################################################################
    // Private Constants

    /** The line separator */
    private static final String LINE = System.getProperty("line.separator");

    /** The default width of an indentation level */
    private static final String STEP = "    ";

    //########################################################################
    // Private Fields

    /** The output buffer */
    private final StringBuffer buffer;

    /** The width of an indentation level */
    private final String step;

    /** The current indentation level */
    private int level;

    /** Do we need to align? */
    private boolean align;

    /** Do we need to print a white space? */
    private boolean space;

    /** Is there already a empty line? */
    private boolean line;

    //########################################################################
    // Public Constructors

    /** Creates a new TextWriter. */
    public TextWriter() {
        this(new StringBuffer());
    }

    /** Creates a new TextWriter. */
    public TextWriter(String step) {
        this(new StringBuffer(), step);
    }

    /** Creates a new TextWriter. */
    public TextWriter(String step, int level) {
        this(new StringBuffer(), step, level);
    }

    /** Creates a new TextWriter. */
    public TextWriter(StringBuffer buffer) {
        this(buffer, STEP, 0);
    }

    /** Creates a new TextWriter. */
    public TextWriter(StringBuffer buffer, String step) {
        this(buffer, step, 0);
    }

    /** Creates a new TextWriter. */
    public TextWriter(StringBuffer buffer, String step, int level) {
        this.buffer = buffer;
        this.step = step;
        this.level = level;
        this.align = false;
        this.space = false;
        this.line = false;
    }

    //########################################################################
    // Public Methods - Printing

    /** Prints a new line. */
    public TextWriter println() {
        return line();
    }

    /** Prints the boolean value followed by a new line. */
    public TextWriter println(boolean value) {
        return print(value).line();
    }

    /** Prints the byte value followed by a new line. */
    public TextWriter println(byte value) {
        return print(value).line();
    }

    /** Prints the short value followed by a new line. */
    public TextWriter println(short value) {
        return print(value).line();
    }

    /** Prints the char value followed by a new line. */
    public TextWriter println(char value) {
        return print(value).line();
    }

    /** Prints the int value followed by a new line. */
    public TextWriter println(int value) {
        return print(value).line();
    }

    /** Prints the long value followed by a new line. */
    public TextWriter println(long value) {
        return print(value).line();
    }

    /** Prints the float value followed by a new line. */
    public TextWriter println(float value) {
        return print(value).line();
    }

    /** Prints the double value followed by a new line. */
    public TextWriter println(double value) {
        return print(value).line();
    }

    /** Prints the string followed by a new line. */
    public TextWriter println(String value) {
        return print(value).line();
    }

    /** Prints the boolean value. */
    public TextWriter print(boolean value) {
        return print(String.valueOf(value));
    }

    /** Prints the byte value. */
    public TextWriter print(byte value) {
        return print(String.valueOf(value));
    }

    /** Prints the short value. */
    public TextWriter print(short value) {
        return print(String.valueOf(value));
    }

    /** Prints the char value. */
    public TextWriter print(char value) {
        return print(String.valueOf(value));
    }

    /** Prints the int value. */
    public TextWriter print(int value) {
        return print(String.valueOf(value));
    }

    /** Prints the long value. */
    public TextWriter print(long value) {
        return print(String.valueOf(value));
    }

    /** Prints the float value. */
    public TextWriter print(float value) {
        return print(String.valueOf(value));
    }

    /** Prints the long value. */
    public TextWriter print(double value) {
        return print(String.valueOf(value));
    }

    /** Prints the string. */
    public TextWriter print(String value) {
        if (align) for (int i = 0; i < level; i++) buffer.append(step);
        if (space) buffer.append(' ');
        buffer.append(value);
        align = false;
        space = false;
        line = false;
        return this;
    }

    //########################################################################
    // Public Methods - Formating

    /** Returns the indentation width. */
    public int getIndentWidth() {
        return step == null ? -1 : step.length();
    }

    /** Returns the indentation level. */
    public int getIndentLevel() {
        return level;
    }

    /** Returns the indentation level. */
    public TextWriter setIndentLevel(int level) {
        this.level = level;
        return this;
    }

    /** Increases the indentation level by one. */
    public TextWriter indent() {
        level++;
        return this;
    }

    /** Decreases the indentation level by one. */
    public TextWriter undent() {
        level--;
        return this;
    }

    /** Starts a new line. */
    public TextWriter line() {
        if (step == null) return space();
        if (line) return this;
        buffer.append(LINE);
        line = align;
        align = true;
        space = false;
        return this;
    }

    /** Inserts a white space. */
    public TextWriter space() {
        space = !align;
        return this;
    }

    //########################################################################
    // Public Methods - Accessing

    /** Returns the underlying StringBuffer. */
    public StringBuffer getBuffer() {
        return buffer;
    }

    /** Returns the generated string. */
    public String toString() {
        return buffer.toString();
    }

    //########################################################################
}
