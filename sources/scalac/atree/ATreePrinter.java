/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.atree;

import ch.epfl.lamp.util.CodePrinter;

import scalac.Unit;
import scalac.Global;
import scalac.symtab.Type;
import scalac.symtab.Symbol;
import scalac.symtab.SymbolTablePrinter;
import scalac.util.Debug;

/** This class provides methods to print attributed trees. */
public class ATreePrinter {

    //########################################################################
    // Private Fields

    /** The global environment */
    private final Global global;

    /** The underlying code printer */
    private final CodePrinter printer;

    /** The underlying symbol table printer */
    private final SymbolTablePrinter symtab;

    //########################################################################
    // Public Constructors

    /** Initalizes this instance */
    public ATreePrinter() {
        this(new CodePrinter());
    }

    /** Initalizes this instance */
    public ATreePrinter(String step) {
        this(Global.instance, new CodePrinter(step));
    }

    /** Initalizes this instance */
    public ATreePrinter(CodePrinter printer) {
        this(Global.instance, printer);
    }

    /** Initalizes this instance */
    public ATreePrinter(Global global, CodePrinter printer) {
        this.global = global;
        this.printer = printer;
        this.symtab = new SymbolTablePrinter(global, printer);
    }

    //########################################################################
    // Public Methods - Getting & Setting

    /** Returns the underlying code printer. */
    public CodePrinter getCodePrinter() {
        return printer;
    }

    //########################################################################
    // Public Methods - Formatting

    /** Increases the indentation level by one. */
    public ATreePrinter indent() {
        printer.indent();
        return this;
    }

    /** Decreases the indentation level by one. */
    public ATreePrinter undent() {
        printer.undent();
        return this;
    }

    /** Inserts a new line. */
    public ATreePrinter line() {
        printer.line();
        return this;
    }

    /** Inserts a white space. */
    public ATreePrinter space() {
        printer.space();
        return this;
    }

    /** Prints an opening brace followed by a new line. */
    public ATreePrinter lbrace() {
        return space().println('{').indent();
    }

    /** Prints a closing brace followed by a new line. */
    public ATreePrinter rbrace() {
        return undent().space().println('}');
    }

    //########################################################################
    // Public Methods - Printing simple values

    /** Prints a new line. */
    public ATreePrinter println() {
        printer.println();
        return this;
    }

    /** Prints the boolean value followed by a new line. */
    public ATreePrinter println(boolean value) {
        printer.println(value);
        return this;
    }

    /** Prints the byte value followed by a new line. */
    public ATreePrinter println(byte value) {
        printer.println(value);
        return this;
    }

    /** Prints the short value followed by a new line. */
    public ATreePrinter println(short value) {
        printer.println(value);
        return this;
    }

    /** Prints the char value followed by a new line. */
    public ATreePrinter println(char value) {
        printer.println(value);
        return this;
    }

    /** Prints the int value followed by a new line. */
    public ATreePrinter println(int value) {
        printer.println(value);
        return this;
    }

    /** Prints the long value followed by a new line. */
    public ATreePrinter println(long value) {
        printer.println(value);
        return this;
    }

    /** Prints the float value followed by a new line. */
    public ATreePrinter println(float value) {
        printer.println(value);
        return this;
    }

    /** Prints the double value followed by a new line. */
    public ATreePrinter println(double value) {
        printer.println(value);
        return this;
    }

    /** Prints the string followed by a new line. */
    public ATreePrinter println(String value) {
        printer.println(value);
        return this;
    }

    /** Prints the boolean value. */
    public ATreePrinter print(boolean value) {
        printer.print(value);
        return this;
    }

    /** Prints the byte value. */
    public ATreePrinter print(byte value) {
        printer.print(value);
        return this;
    }

    /** Prints the short value. */
    public ATreePrinter print(short value) {
        printer.print(value);
        return this;
    }

    /** Prints the char value. */
    public ATreePrinter print(char value) {
        printer.print(value);
        return this;
    }

    /** Prints the int value. */
    public ATreePrinter print(int value) {
        printer.print(value);
        return this;
    }

    /** Prints the long value. */
    public ATreePrinter print(long value) {
        printer.print(value);
        return this;
    }

    /** Prints the float value. */
    public ATreePrinter print(float value) {
        printer.print(value);
        return this;
    }

    /** Prints the long value. */
    public ATreePrinter print(double value) {
        printer.print(value);
        return this;
    }

    /** Prints the string. */
    public ATreePrinter print(String value) {
        printer.print(value);
        return this;
    }

    //########################################################################
    // Public Methods - Printing types and symbols

    /** Prints the symbol. */
    public ATreePrinter printSymbol(Symbol symbol) {
        symtab.printSymbolName(symbol);
        return this;
    }

    /** Prints the type. */
    public ATreePrinter printType(Type type) {
        symtab.printType(type);
        return this;
    }

    //########################################################################
    // Public Methods - Converting

    /** Returns the string representation of this printer. */
    public String toString() {
        return printer.toString();
    }

    //########################################################################
}
