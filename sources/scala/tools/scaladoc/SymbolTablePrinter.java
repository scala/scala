/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.scaladoc;

import java.io.StringWriter;

import ch.epfl.lamp.util.CodePrinter;
import ch.epfl.lamp.util.HTMLPrinter;

import scalac.Global;
import scalac.atree.AConstant;
import scalac.symtab.*;
import scalac.symtab.Scope.SymbolIterator;
import scalac.symtab.Type.Constraint;
import scalac.util.Debug;
import scalac.util.Name;
import scalac.util.NameTransformer;

// Lines modified or added by Vincent are marked "vincent".

/**
 * This class provides methods to print symbols and types.
 */
public abstract class MySymbolTablePrinter extends SymbolTablePrinter {

    //########################################################################
    // Private Fields

    /** The global environment */
    protected final Global global;

    //########################################################################
    // Public Constructors

    /**
     * Creates a new instance.
     *
     * @param page
     */
    public MySymbolTablePrinter(CodePrinter printer) {
        super(printer);
        this.global = Global.instance;
    }

    public abstract void printSymbol(Symbol sym, boolean addLink);

    //########################################################################
    // Public Methods - Printing symbols

    /**
     * Prints the name of the given symbol usage.
     *
     * @param symbol
     */
    public SymbolTablePrinter printSymbolName(Symbol symbol) {
	printSymbol(symbol, true);
        printSymbolUniqueId(symbol);
        return this;
    }

    /**
     * Prints the name of the given symbol definition.
     *
     * @param symbol
     * @param addLink
     */
    public SymbolTablePrinter printDefinedSymbolName(Symbol symbol, boolean addLink) {
	printSymbol(symbol, addLink);
        printSymbolUniqueId(symbol);
        return this;
    }

    public SymbolTablePrinter printSeqType(Symbol symbol, Type t, String inner) {
        Type type = t;
        boolean star = false;
        if ((symbol.flags & Modifiers.REPEATED) != 0 &&
            type.symbol() == global.definitions.SEQ_CLASS &&
            type.typeArgs().length == 1)
            {
                type = type.typeArgs()[0];
                star = true;
            }
	printType(type, inner);
        if (star) print("*");
        return this;
    }

    /**
     * Writes the string representation of the signature of a definition.
     *
     * @param sym
     * @param addLink Generates an hypertext reference if the parameter
     *        <code>addLink</code> is <code>true</code>
     */
    public void defString(Symbol sym, boolean addLink) {
	if (sym.isRoot())
	    print("Root class");
	else if (sym.isClass() || sym.isModule())
	    printTemplateSignature(sym, addLink);
	else if (sym.isType() && !sym.isParameter())
	    printShortSignature(sym, addLink);
	else
	    printSignature(sym, addLink);
    }

    public SymbolTablePrinter printSignature(Symbol symbol) {
        return printSignature(symbol, false);
    }

    /**
     * Prints the signature of the given symbol.
     *
     * @param symbol
     * @param addLink
     */
    public SymbolTablePrinter printSignature(Symbol symbol, boolean addLink) {
	return printShortSignature(symbol, addLink)
	    .printSymbolType(symbol, getSymbolInnerString(symbol));
    }

    public SymbolTablePrinter printValueParams(Symbol[] vparams) {
        print('(');
        for (int i = 0; i < vparams.length; i++) {
            if (i > 0) print(", ");
            if (vparams[i].isDefParameter()) print("def ");
            defString(vparams[i], false);
        }
        return print(')');
    }

    /** The keyword of a symbol from the user side. */
    public String getSymbolKeywordForDoc(Symbol symbol) {
        String keyword = getSymbolKeyword(symbol);
        // package
        if (symbol.isPackage()) keyword = "package";
        // accessor function for a val
        if (symbol.isInitializedMethod() && (symbol.flags & Modifiers.STABLE) != 0)
            keyword = "val";
        return keyword;
    }

    /**
     * Prints the signature of the given symbol.
     *
     * @param symbol
     * @param addLink
     */
    public SymbolTablePrinter printShortSignature(Symbol symbol, boolean addLink) {
        String keyword = getSymbolKeywordForDoc(symbol);
        if (keyword != null) print(keyword).space();
	printSymbol(symbol, addLink);
        return printType(symbol.loBound(), ">:");
    }

    /**
     * Prints the signature of a class symbol.
     *
     * @param symbol
     * @param addLink
     * @return the current symbol table printer
     */
    public SymbolTablePrinter printTemplateSignature(Symbol symbol, boolean addLink) {
        // kind
	String keyword = getSymbolKeywordForDoc(symbol);
        if (keyword != null) print(keyword).space();
        String inner = getSymbolInnerString(symbol);

	// name
	printDefinedSymbolName(symbol, addLink);
	if (symbol.isClass()) {
	    // type parameters
	    Symbol[] tparams = symbol.typeParams();
	    if (tparams.length != 0 || global.debug) printTypeParams(tparams);
	    // value parameters
	    Symbol[] vparams = symbol.valueParams();
            printValueParams(vparams);
	}

	// parents
// 	Type[] parts = symbol.moduleClass().parents();
// 	if (parts.length != 0) space().print(inner).space();
// 	printTypes(parts," with ");
	return this;
    }

    //########################################################################
    // Public Methods - Printing constants

    /** Prints the given constant value. */
    public SymbolTablePrinter printConstantValue(AConstant value) {
        return this;
    }

    //########################################################################
}
