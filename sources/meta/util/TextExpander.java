/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package meta.util;

import java.io.File;
import java.io.FileReader;
import java.io.BufferedReader;
import java.io.IOException;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.InvocationTargetException;

/** A macro expander with support for indentation */
public class TextExpander {

    //########################################################################
    // Public Constants

    /** Default macro name prefix */
    public static final String MACRO_PREFIX = "print";

    /** Default macro left quote */
    public static final String MACRO_LQUOTE = "{#";

    /** Default macro right quote */
    public static final String MACRO_RQUOTE = "#}";

    /** Do not edit warning */
    public static final String DO_NOT_EDIT =
        "DO NOT EDIT. Automatically generated file!";

    //########################################################################
    // Private Constants

    /** An empty array of Classes */
    private static final Class [] NO_PARAMS = new Class[0];

    /** An empty array of Objects */
    private static final Object[] NO_ARGS   = new Object[0];

    //########################################################################
    // Private Fields

    /** The output text writer */
    private final TextWriter writer;

    /** The macro expander */
    private final Object expander;

    /** The current macro name prefix */
    private String prefix;

    /** The current macro left quote */
    private String lquote;

    /** The current macro right quote */
    private String rquote;

    /** The current input name */
    private String name;

    /** The current input line number */
    private int line;

    /** The current error count */
    private int errors;

    //########################################################################
    // Public Constructors

    /** Creates a new TextExpander. */
    public TextExpander(TextWriter writer, Object expander) {
        this.writer = writer;
        this.expander = expander;
        this.prefix = MACRO_PREFIX;
        this.lquote = MACRO_LQUOTE;
        this.rquote = MACRO_RQUOTE;
    }

    //########################################################################
    // Public Methods - Text expansion

    /** Expands the text from the given file. */
    public void expandText(File file) {
        setInputName(file.toString());
        setInputLine(0);
        try {
            BufferedReader reader = new BufferedReader(new FileReader(file));
            expandText(reader);
            reader.close();
        } catch (IOException exception) {
            error("input error", exception);
        }
    }

    /** Expands the text from the given character stream. */
    public void expandText(BufferedReader reader) {
        try {
            for (String line; (line = reader.readLine()) != null;) {
                this.line++;
                expandLine(line);
            }
        } catch (IOException exception) {
            error("input error", exception);
        }
    }

    /** Expands the given line. */
    public void expandLine(String line) {
        int index = 0;
        int column = 0;
        for (; index < line.length(); index++) {
            switch (line.charAt(index)) {
            case ' ' : column += 1; continue;
            case '\t': column += 8 - (column % 8);  continue;
            }
            break;
        }
        if (index < line.length()) {
            int width = writer.getIndentWidth();
            writer.setIndentLevel(column / width);
            for (int i = 0; i < column % width; i++) writer.print(' ');
            line = line.substring(index);
            if (line.indexOf("$Id") >= 0)
                line = line.replaceFirst("[$]Id.*[$]", DO_NOT_EDIT);
            expand(line);
        }
        writer.println();
    }

    /** Expands the given string. */
    public void expand(String input) {
        int start = 0;
        while (true) {
            int lindex = input.indexOf(lquote, start);
            if (lindex < 0) break;
            int rindex = input.indexOf(rquote, lindex + lquote.length());
            if (rindex < 0) { error("unclosed macro"); break; }
            String macro = input.substring(lindex + lquote.length(), rindex);
            if (macro.indexOf(lquote) >= 0) { error("unclosed macro"); break; }
            if (start < lindex) writer.print(input.substring(start, lindex));
            expandMacro(macro);
            start = rindex + rquote.length();
        }
        if (start < input.length()) writer.print(input.substring(start));
    }

    //########################################################################
    // Public Methods - Macro expansion

    /** Expands the given macro. */
    public void expandMacro(String macro) {
        String member = prefix + macro;
        Class clasz = expander.getClass();
        try {
            expandMacro(macro, clasz.getMethod(member, NO_PARAMS));
        } catch (NoSuchMethodException exception) {
            error("macro '" + macro + "' is undefined");
            writer.print(lquote + macro + rquote);
        }
    }

    /** Expands the given macro by invoking the given method. */
    public void expandMacro(String macro, Method method) {
        try {
            method.invoke(expander, NO_ARGS);
        } catch (IllegalAccessException exception) {
            error("macro '" + macro + "' could not access method " + method);
            writer.print(lquote + macro + rquote);
        } catch (InvocationTargetException exception) {
            error("macro '" + macro + "' raised an exception");
            exception.getTargetException().printStackTrace();
        }
    }

    //########################################################################
    // Public Methods - Error messages

    /** Prints an error message. */
    public void error(String error) {
        String prefix = name != null ? name + ":" : "error:";
        if (line > 0) prefix = prefix + line + ":";
        System.err.println(prefix + " " + error);
        errors++;
    }

    /** Prints an error message. */
    public void error(String error, Throwable exception) {
        if (exception.getMessage() != null)
            error(exception.getMessage());
        else
            error(error + " (" + exception.getClass().getName() + ")");
    }

    //########################################################################
    // Public Methods - Getters

    /** Returns the output text writer. */
    public TextWriter getOutputWriter() {
        return writer;
    }

    /** Returns the macro expander. */
    public Object getMacroExpander() {
        return expander;
    }

    /** Returns the current macro name prefix. */
    public String getMacroPrefix() {
        return prefix;
    }

    /** Returns the current macro left quote. */
    public String getMacroLeftQuote() {
        return lquote;
    }

    /** Returns the current macro right quote. */
    public String getMacroRightQuote() {
        return lquote;
    }

    /** Returns the current input name. */
    public String getInputName() {
        return name;
    }

    /** Returns the current input line. */
    public int getInputLine() {
        return line;
    }

    /** Returns the current error count. */
    public int getErrorCount() {
        return errors;
    }

    //########################################################################
    // Public Methods - Setters

    /** Sets the current macro name prefix. */
    public void setMacroPrefix(String prefix) {
        this.prefix = prefix;
    }

    /** Sets the current macro quotes. */
    public void setMacroQuotes(String lquote, String rquote) {
        this.lquote = lquote;
        this.rquote = rquote;
    }

    /** Sets the current input name. */
    public void setInputName(String name) {
        this.name = name;
    }

    /** Sets the current input line. */
    public void setInputLine(int line) {
        this.line = line;
    }

    /** Sets the current error count. */
    public void setErrorCount(int errors) {
        this.errors = errors;
    }

    //########################################################################
}
