/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package meta.java;

import java.util.Iterator;
import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.HashSet;
import java.util.TreeSet;

import meta.util.TextWriter;
import meta.util.TextExpander;

/** A string generator with support for java source code generation. */
public class JavaWriter {

    //########################################################################
    // Public Constants

    /** The width of a separator */
    public static final int SEPARATOR_WIDTH = 78;

    //########################################################################
    // Private Fields

    /** The underlying text writer */
    private final TextWriter writer;

    /** The current package */
    private final String peckage;

    /** List of imports on-demand */
    private final Set/*<String>*/ owners;

    /** List of explicit imports (maps short names to full names) */
    private final Map/*<String,String>*/ types;

    //########################################################################
    // Public Constructors

    /** Creates a new JavaPrinter with no current package. */
    public JavaWriter() {
        this((String)null);
    }

    /** Creates a new JavaPrinter with the given current package. */
    public JavaWriter(String peckage) {
        this(new TextWriter("    "), peckage);
    }

    /** Creates a new JavaPrinter with no current package. */
    public JavaWriter(TextWriter writer) {
        this(writer, null);
    }

    /** Creates a new JavaPrinter with the given current package. */
    public JavaWriter(TextWriter writer, String peckage) {
        this.writer = writer;
        this.peckage = peckage;
        this.owners = new HashSet();
        this.types = new HashMap();
    }

    //########################################################################
    // Public Methods - Importing types

    /** Returns the current package. */
    public String getPackage() {
        return peckage;
    }

    /** Returns true if the given type needs to be fully qualified. */
    public boolean needsQualification(Type type) {
        type = type.getBaseType();
        String owner = type.getOwner();
        if (owner == null) return true;
        Object current = types.get(type.getName());
        if (current != null) return !type.getFullName().equals(current);
        return !owner.equals(getPackage()) && !owners.contains(owner);
    }

    /** If necessary, adds an explicit import for the given type. */
    public void importType(Type type) {
        importType(type, false);
    }

    /** If necessary, adds an explicit import for the given type. */
    public void importType(Type type, boolean force) {
        type = type.getBaseType();
        String owner = type.getOwner();
        if (owner == null) return;
        if (!force && owner.equals(getPackage())) return;
        if (!force && owners.contains(owner)) return;
        String shortname = type.getName();
        String longname = type.getFullName();
        Object current = types.get(shortname);
        if (current == null) {
            types.put(shortname, longname);
        } else if (!longname.equals(current)) {
            throw new Error();
        }
    }

    /** Adds an import on demand for members of the given type. */
    public void importFrom(Type type) {
        importFrom(type.getFullName());
    }

    /** Adds an import on demand for members of the given owner. */
    public void importFrom(String owner) {
        owners.add(owner);
    }

    //########################################################################
    // Public Methods - Printing java statements and expressions

    /** Prints a package statement for the current package. */
    public JavaWriter printPackage() {
        return getPackage() == null ? this : printPackage(getPackage());
    }

    /** Prints a package statement. */
    public JavaWriter printPackage(String peckage) {
        return print("package ").print(peckage).println(";");
    }

    /** Prints an import statement. */
    public JavaWriter printImport(String inport) {
        return print("import ").print(inport).println(";");
    }

    /** Prints import statements for the current imports. */
    public JavaWriter printImports() {
        return printImports(getImports());
    }

    /** Prints a list of import statements. */
    public JavaWriter printImports(Set imports) {
        for (Iterator i = imports.iterator(); i.hasNext(); )
            printImport((String)i.next());
        if (imports.size() > 0) println();
        return this;
    }

    /** Prints a "do not edit" comment. */
    public JavaWriter printDoNotEdit() {
        return printComment(TextExpander.DO_NOT_EDIT);
    }

    /** Prints a single-line Java documentation comment. */
    public JavaWriter printDescription(String line) {
        return print("/** ").print(line).println(" */");
    }

    /** Prints a multi-line Java documentation comment. */
    public JavaWriter printDescription(String[] lines) {
        println("/**");
        for (int i = 0; i < lines.length; i++) print(" * ").println(lines[i]);
        return println(" */");
    }

    /** Prints a single-line comment. */
    public JavaWriter printComment(String comment) {
        return print("// ").println(comment);
    }

    /** Prints a separator. */
    public JavaWriter printSeparator() {
        println().print("//");
        int width = SEPARATOR_WIDTH - 2 - getIndentLevel() * getIndentWidth();
        for (int i = 0; i < width; i++) print("#");
        return println();
    }

    /** Prints a separator with the given title. */
    public JavaWriter printSeparator(String title) {
        return printSeparator().printComment(title).println();
    }

    /** Prints a separator with the given title and subtitle. */
    public JavaWriter printSeparator(String title, String subtitle) {
        return printSeparator(title + " - " + subtitle);
    }

    //########################################################################
    // Public Methods - Printing simple values

    /** Prints a new line. */
    public JavaWriter println() {
        return line();
    }

    /** Prints the boolean value followed by a new line. */
    public JavaWriter println(boolean value) {
        return print(value).line();
    }

    /** Prints the byte value followed by a new line. */
    public JavaWriter println(byte value) {
        return print(value).line();
    }

    /** Prints the short value followed by a new line. */
    public JavaWriter println(short value) {
        return print(value).line();
    }

    /** Prints the char value followed by a new line. */
    public JavaWriter println(char value) {
        return print(value).line();
    }

    /** Prints the int value followed by a new line. */
    public JavaWriter println(int value) {
        return print(value).line();
    }

    /** Prints the long value followed by a new line. */
    public JavaWriter println(long value) {
        return print(value).line();
    }

    /** Prints the float value followed by a new line. */
    public JavaWriter println(float value) {
        return print(value).line();
    }

    /** Prints the double value followed by a new line. */
    public JavaWriter println(double value) {
        return print(value).line();
    }

    /** Prints the string followed by a new line. */
    public JavaWriter println(String value) {
        return print(value).line();
    }

    /** Prints the type followed by a new line. */
    public JavaWriter println(Type value) {
        return print(value).line();
    }

    /** Prints the boolean value. */
    public JavaWriter print(boolean value) {
        writer.print(value);
        return this;
    }

    /** Prints the byte value. */
    public JavaWriter print(byte value) {
        writer.print(value);
        return this;
    }

    /** Prints the short value. */
    public JavaWriter print(short value) {
        writer.print(value);
        return this;
    }

    /** Prints the char value. */
    public JavaWriter print(char value) {
        writer.print(value);
        return this;
    }

    /** Prints the int value. */
    public JavaWriter print(int value) {
        writer.print(value);
        return this;
    }

    /** Prints the long value. */
    public JavaWriter print(long value) {
        writer.print(value);
        return this;
    }

    /** Prints the float value. */
    public JavaWriter print(float value) {
        writer.print(value);
        return this;
    }

    /** Prints the long value. */
    public JavaWriter print(double value) {
        writer.print(value);
        return this;
    }

    /** Prints the string. */
    public JavaWriter print(String value) {
        writer.print(value);
        return this;
    }

    /** Prints the type. */
    public JavaWriter print(Type value) {
        return print(value.getName(needsQualification(value)));
    }

    //########################################################################
    // Public Methods - Formating

    /** Returns the indentation width. */
    public int getIndentWidth() {
        return writer.getIndentWidth();
    }

    /** Returns the indentation level. */
    public int getIndentLevel() {
        return writer.getIndentLevel();
    }

    /** Returns the indentation level. */
    public JavaWriter setIndentLevel(int level) {
        writer.setIndentLevel(level);
        return this;
    }

    /** Increases the indentation level by one. */
    public JavaWriter indent() {
        writer.indent();
        return this;
    }

    /** Decreases the indentation level by one. */
    public JavaWriter undent() {
        writer.undent();
        return this;
    }

    /** Starts a new line. */
    public JavaWriter line() {
        writer.line();
        return this;
    }

    /** Inserts a white space. */
    public JavaWriter space() {
        writer.space();
        return this;
    }

    /** Prints an opening brace followed by a new line. */
    public JavaWriter lbrace() {
        return space().println("{").indent();
    }

    /** Prints a closing brace followed by a new line. */
    public JavaWriter rbrace() {
        return undent().space().println("}");
    }

    //########################################################################
    // Public Methods - Accessing

    /** Returns the underlying StringBuffer. */
    public TextWriter getTextWriter() {
        return writer;
    }

    /** Returns the underlying StringBuffer. */
    public StringBuffer getBuffer() {
        return writer.getBuffer();
    }

    /** Returns the generated string. */
    public String toString() {
        return writer.toString();
    }

    //########################################################################
    // Private Methods

    /** Returns the current list of imports (explicit and on-demand). */
    private Set getImports() {
        Set imports = new TreeSet();
        for (Iterator i = owners.iterator(); i.hasNext(); )
            imports.add(i.next() + ".*");
        for (Iterator i = types.values().iterator(); i.hasNext(); )
            imports.add(i.next());
        return imports;
    }

    //########################################################################
}
