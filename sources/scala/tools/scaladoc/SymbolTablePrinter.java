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
public class SymbolTablePrinter extends scalac.symtab.SymbolTablePrinter {

    //########################################################################
    // Private Fields

    /** The global environment */
    private final Global global;

    /** The HTML generator using this object */ // vincent
    HTMLGenerator htmlGenerator;

    //########################################################################
    // Public Constructors

    /**
     * Creates a new instance.
     *
     * @param htmlgenerator
     * @param page
     */
    public SymbolTablePrinter(HTMLGenerator generator, CodePrinter printer) {
        super(printer);
        this.global = Global.instance;
        this.htmlGenerator = generator;
    }

    //########################################################################
    // Public Methods - Printing scopes

    /**
     * Prints the members of the given scope.
     *
     * @param scope
     */
    public SymbolTablePrinter printScope(Scope scope) {
        return printScope(scope, false);
    }

    /**
     * Prints the members of the given scope.
     *
     * @param scope
     * @param lazy
     */
    public SymbolTablePrinter printScope(Scope scope, boolean lazy) {
        boolean first = true;
        for (SymbolIterator i = scope.iterator(); i.hasNext(); ) {
            Symbol member = i.next();
            if (!mustShowMember(member)) continue;
            if (first) print("{").indent(); else print(", ");
            first = false;
            line();
            printSignature(member, false);
        }
        if (!first)
            line().undent().print("}");
        else if (!lazy)
            print("{}");
        return this;
    }

    /**
     * Returns <code>true</code> iff the given member must be printed.
     * The default implementation always returns <code>true</code>.
     *
     * @param member
     */
    public boolean mustShowMember(Symbol member) {
        return true;
    }

    //########################################################################
    // Public Methods - Printing symbols

    /**
     * Returns the inner string of the given symbol.
     *
     * @param symbol
     */
    public String getSymbolInnerString(Symbol symbol) {
        if (symbol.kind == Kinds.TYPE)
            return INNER_LT; // HTML encoded "<:" symbol
        else
            return super.getSymbolInnerString(symbol);
    }
    // where
    protected String INNER_LT = "&lt;:";

    /**
     * Prints the name of the given symbol usage.
     *
     * @param symbol
     */
    public SymbolTablePrinter printUsedSymbolName(Symbol symbol) {
	htmlGenerator.printSymbol(symbol, true);
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
	htmlGenerator.printSymbol(symbol, addLink);
        printSymbolUniqueId(symbol);
        return this;
    }

    /**
     * Prints the type of the given symbol with the given inner string.
     *
     * @param symbol
     * @param inner
     */
    public SymbolTablePrinter printSymbolType(Symbol symbol, String inner) {
        Type type = symbol.rawFirstInfo();
        if (!(type instanceof Type.LazyType)) type = symbol.info();
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
     * Prints the signature of the given symbol.
     *
     * @param symbol
     * @param addLink
     */
    public SymbolTablePrinter printSignature(Symbol symbol, boolean addLink) {
        String keyword = getSymbolKeyword(symbol);
        if (keyword != null) print(keyword).space();
        String inner = getSymbolInnerString(symbol);
	htmlGenerator.printSymbol(symbol, addLink);
        return printType(symbol.loBound(), ">:")
	    .printSymbolType(symbol, inner);
    }

    /**
     * Prints the signature of the given symbol.
     *
     * @param symbol
     * @param addLink
     */
    public SymbolTablePrinter printShortSignature(Symbol symbol, boolean addLink) {
        String keyword = getSymbolKeyword(symbol);
        if (keyword != null) print(keyword).space();
	htmlGenerator.printSymbol(symbol, addLink);
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
	String keyword = getSymbolKeyword(symbol);
        if (keyword != null) print(keyword).space();
        String inner = getSymbolInnerString(symbol);

	// name
	printDefinedSymbolName(symbol, addLink);
	if (symbol.isClass()) {
	    // type parameters
	    Symbol[] tparams = symbol.typeParams();
	    if (tparams.length != 0 || global.debug) {
		print('[');
		for (int i = 0; i < tparams.length; i++) {
		    if (i > 0) print(",");
		    printSignature(tparams[i], false);
		}
		print(']');
	    }
	    // value parameters
	    Symbol[] vparams = symbol.valueParams();
	    print('(');
	    for (int i = 0; i < vparams.length; i++) {
		if (i > 0) print(", "); // vincent
		if (vparams[i].isDefParameter()) print("def ");
		htmlGenerator.defString(vparams[i], false);
	    }
	    print(')');
	}

	// parents
// 	Type[] parts = symbol.moduleClass().parents();
// 	if (parts.length != 0) space().print(inner).space();
// 	printTypes(parts," with ");
	return this;
    }

    /**
     * Prints the signature of a class symbol.
     *
     * @param symbol
     * @param addLink
     */
    public SymbolTablePrinter printTemplateHtmlSignature(Symbol symbol, boolean addLink) {
	// modifiers
        String mods = Modifiers.Helper.toString(symbol.flags);
	htmlGenerator.page.printlnOTag("dl");
	htmlGenerator.page.printlnOTag("dt");
	print(mods).space();

        // kind
	String keyword = getSymbolKeyword(symbol);
        if (keyword != null) print(keyword).space();
        String inner = getSymbolInnerString(symbol);

        // name
	printDefinedSymbolName(symbol, addLink);
	if (symbol.isClass()) {
	    // type parameters
	    Symbol[] tparams = symbol.typeParams();
	    if (tparams.length != 0 || global.debug) {
		print('[');
		for (int i = 0; i < tparams.length; i++) {
		    if (i > 0) print(",");
		    printSignature(tparams[i], false);
		}
		print(']');
	    }
	    // value parameters
	    Symbol[] vparams = symbol.valueParams();
	    print('(');
	    for (int i = 0; i < vparams.length; i++) {
		if (i > 0) print(", ");
		if (vparams[i].isDefParameter()) print("def ");
		htmlGenerator.defString(vparams[i], false);
	    }
	    print(')');
	}

        // parents
        Type[] parts = symbol.moduleClass().parents();
        htmlGenerator.page.printlnCTag("dt");
        for (int i = 0; i < parts.length; i++) {
            htmlGenerator.page.printOTag("dd");
            print((i == 0) ? "extends " : "with ");
            printType(parts[i]);
	    htmlGenerator.page.printlnCTag("dd");
	}
	htmlGenerator.page.printCTag("dl");
	return this;
    }

    //########################################################################
    // Public Methods - Printing types

    /**
     * Prints the given types separated by infix.
     *
     * @param types
     * @param infix
     */
    public SymbolTablePrinter printTypes(Type[] types, String infix) {
        for (int i = 0; i < types.length; i++) {
            if (i > 0) print(infix);
            printType(types[i]);
        }
        return this;
    }

    /**
     * Prints the given type.
     *
     * @param type
     */
    public SymbolTablePrinter printType(Type type) {
        return printType0(getTypeToPrintForType(type));
    }

    /**
     * ..
     *
     * @param type
     */
    public SymbolTablePrinter printType0(Type type) {
        printCommonPart(type);
        switch (type) {
        case ThisType(_):
        case SingleType(_,_):
            print(".type");
            return this;
        default:
            return this;
        }
    }

    /**
     * Prints the given type with the given inner string.
     *
     * @param type
     * @param inner
     */
    public SymbolTablePrinter printType(Type type, String inner) {
	if ((INNER_LT.equals(inner) && type.symbol() == global.definitions.ANY_CLASS) ||
	    (">:".equals(inner) && type.symbol() == global.definitions.ALL_CLASS))
	    return this;
        else
	    return printType0(getTypeToPrintForType(type), inner);
    }

    /**
     * ..
     *
     * @param type
     * @param inner
     */
    public SymbolTablePrinter printType0(Type type, String inner) {
        switch (type) {
        case MethodType(Symbol[] vparams, Type result):
            print('(');
            for (int i = 0; i < vparams.length; i++) {
                if (i > 0) print(", ");
		if (vparams[i].isDefParameter()) print("def ");
		htmlGenerator.defString(vparams[i], false);
		//print(NameTransformer.decode(vparams[i].name.toString()) + ": ");// vincent
                //printSymbolType(vparams[i], null);
            }
            print(')');
            return printType(result, inner);
        case PolyType(Symbol[] tparams, Type result):
            if (tparams.length != 0 || global.debug) {
                print('[');
                for (int i = 0; i < tparams.length; i++) {
                    if (i > 0) print(",");
                    printSignature(tparams[i], false);
                }
                print(']');
            }
            return printType(result, inner);
        default:
            if (inner != null) {
                if (!inner.startsWith(":")) space();
                print(inner).space();
            }
            return printType0(type);
        }
    }

    /**
     * Prints a function type with the given type arguments.
     *
     * @param types
     */
    public SymbolTablePrinter printFunctionType(Type[] types) {
        Type[] args = new Type[types.length - 1];
        for (int i = 0; i < args.length; i++) args[i] = types[i];
        print('(');
        printTypes(args, ", ");
        print(") => ");
        printType(types[types.length - 1]);
        return this;
    }

    /**
     * Prints a template type with the given base types.
     *
     * @param types
     */
    public SymbolTablePrinter printTemplateType(Type[] types) {
        print("<template: ");
        printTypes(types, " with ");
        print(" {...}>");
        return this;
    }

    /**
     * Prints the type and prefix common part of the given type.
     *
     * @param type
     */
    public SymbolTablePrinter printCommonPart(Type type) {
        switch (type) {
	case ErrorType:
	    print("<error>");
            return this;

	case AnyType:
	    print("<any type>");
            return this;

	case NoType:
	    print("<notype>");
            return this;

	case ThisType(Symbol sym):
            if (sym == Symbol.NONE) print("<local>.this");
            if ((sym.isAnonymousClass() || sym.isCompoundSym()) && !global.debug)
		print("this");
            printUsedSymbolName(sym);
            print(".this"); // vincent
            return this;

	case TypeRef(Type pre, Symbol sym, Type[] args):
	    if (!global.debug) {
                if (type.isFunctionType()) {
                    printFunctionType(args);
		    return this;
		}
		if (sym.isAnonymousClass() || sym.isCompoundSym())
                    printTemplateType(pre.memberInfo(sym).parents());
            }
            printPrefix(pre, sym);
            printUsedSymbolName(sym);
	    if (args.length != 0) {
                print('[');
                printTypes(args, ",");
                print(']');
            }
            return this;

	case SingleType(Type pre, Symbol sym):
            printPrefix(pre, sym);
            printUsedSymbolName(sym);
            return this;

	case ConstantType(Type base, Object value):
	    printType(base);
	    print("(");
	    print(value.toString());
	    print(")");
	    return this;

	case CompoundType(Type[] parts, Scope members):
	    printTypes(parts," with ");
            space();
            return printScope(members, true); // vincent

	case MethodType(_, _):
	    return printType0(type);

	case PolyType(_, _):
	    return printType0(type);

	case OverloadedType(Symbol[] alts, Type[] alttypes):
            return printTypes(alttypes, " <and> ");

	case TypeVar(Type origin, Constraint constr):
            printType(origin);
            print("?");
            return this;

	case UnboxedType(int kind):
	    print(type.unboxedName(kind).toString());
            return this;

	case UnboxedArrayType(Type elemtp):
	    printType(elemtp);
            print("[]");
            return this;

	case LazyType():
            if (!global.debug) print("?");
            String classname = type.getClass().getName();
            print("<lazy type ").print(classname).print(">");
            return this;

	default:
            String classname = type.getClass().getName();
	    print("<unknown type ").print(classname).print(">");
            return this;
        }
    }

    //########################################################################
    // Public Methods - Printing prefixes

    /**
     * Returns the type to print for the given prefix (transitive).
     *
     * @param prefix
     * @param sym
     */
    public Type getTypeToPrintForPrefix(Type prefix, Symbol sym) {
        while (true) {
            Type result = getTypeToPrintForPrefix0(prefix);
            if (result == prefix || result == null) {
		return
		    htmlGenerator.cleanPrefix(result); // vincent
		// Next lines should replace the previous one in theory.
		// 		    htmlGenerator.isReferenced(sym) ?
		// 		    htmlGenerator.cleanPrefix(result) : // vincent
		// 		    result;
	    }
            prefix = result;
	}
    }

    /**
     * Prints the given type as a prefix.
     *
     * @param prefix
     */
    public SymbolTablePrinter printPrefix(Type prefix, Symbol sym) {
        prefix = getTypeToPrintForPrefix(prefix, sym);
        return prefix == null ? this : printPrefix0(prefix);
    }

    /**
     * Returns the type to print for the given prefix (non-transitive).
     *
     * @param prefix
     */
    public Type getTypeToPrintForPrefix0(Type prefix) {
        if (!global.debug) {
            if (prefix.symbol().kind == Kinds.NONE) return null;
            if (prefix.symbol().isRoot()) return null;
        }
        return getTypeToPrintForType0(prefix);
    }

    /**
     * ..
     *
     * @param prefix
     */
    public SymbolTablePrinter printPrefix0(Type prefix) {
        printCommonPart(prefix);
        switch (prefix) {
        case ThisType(_):
        case SingleType(_,_):
            print(".");
            return this;
        default:
            print("#");
            return this;
        }
    }

    //########################################################################
    // Public Methods - Converting

    /**
     * Returns the string representation of this.
     */
    public String toString() {
        return toString();
    }

    //########################################################################
}
