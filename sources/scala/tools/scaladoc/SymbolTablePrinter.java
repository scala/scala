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

    /** The underlying code printer */
    private final HTMLPrinter page;

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
    public SymbolTablePrinter(HTMLGenerator generator, HTMLPrinter page) {
        super(page.getCodePrinter());
        this.global = Global.instance;
	this.page = page;
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
            printSignature(member, Symbol.NONE, false);
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
            return "&lt;:"; // HTML encoded "<:" symbol
        else
            return super.getSymbolInnerString(symbol);
    }

    /**
     * Prints the name of the given symbol usage.
     *
     * @param symbol
     * @param user
     */
    public SymbolTablePrinter printUsedSymbolName(Symbol symbol, Symbol user) {
        Name name = symbol.name;
        if (!global.debug) name = NameTransformer.decode(name);
        String s = name.toString();
        if (htmlGenerator.isReferenced(symbol))
            page.printAhref(htmlGenerator.ref(symbol, user), s);
	else
	    page.print(s);
        printSymbolUniqueId(symbol);
        return this;
    }

    /**
     * Prints the name of the given symbol definition.
     *
     * @param symbol
     * @param user
     * @param addLink
     */
    public SymbolTablePrinter printDefinedSymbolName(Symbol symbol, Symbol user, boolean addLink) {
        Name name = symbol.name;
        if (!global.debug) name = NameTransformer.decode(name);
        String s = name.toString();
        if (htmlGenerator.isReferenced(symbol))
            if (addLink)
                page.printAhref(htmlGenerator.ref(symbol, user), s);
            else
	        page.printBold(s);
	else
	    page.print(s);
        printSymbolUniqueId(symbol);
        return this;
    }

    /**
     * Prints the type of the given symbol with the given inner string.
     *
     * @param symbol
     * @param inner
     */
    public SymbolTablePrinter printSymbolType(Symbol symbol, String inner, Symbol user) {
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
	printType(type, inner, user);
        if (star) print("*");
        return this;
    }

    /**
     * Prints the signature of the given symbol.
     *
     * @param symbol
     * @param addLink
     */
    public SymbolTablePrinter printSignature(Symbol symbol, Symbol user, boolean addLink) {
        String keyword = getSymbolKeyword(symbol);
        if (keyword != null) print(keyword).space();
        String inner = getSymbolInnerString(symbol);
        String name = symbol.nameString();
        if (addLink)
            page.printAhref("#" + name, name);
	else
            page.print(name);
        return printType(symbol.loBound(), ">:", user)
	    .printSymbolType(symbol, inner, user);
    }

    /**
     * Prints the signature of the given symbol.
     *
     * @param symbol
     * @param addLink
     */
    public SymbolTablePrinter printShortSignature(Symbol symbol, Symbol user, boolean addLink) {
        String keyword = getSymbolKeyword(symbol);
        if (keyword != null) print(keyword).space();
        String inner = getSymbolInnerString(symbol);
        String name = symbol.nameString();
        if (addLink)
            page.printAhref("#" + name, name);
        else
            page.print(name);
        return printType(symbol.loBound(), ">:", user);
    }

    /**
     * Prints the signature of a class symbol.
     *
     * @param symbol
     * @param addLink
     * @return the current symbol table printer
     */
    public SymbolTablePrinter printTemplateSignature(Symbol symbol, Symbol user, boolean addLink) {
        // kind
	String keyword = getSymbolKeyword(symbol);
        if (keyword != null) print(keyword).space();
        String inner = getSymbolInnerString(symbol);

	// name
	printDefinedSymbolName(symbol, user, addLink);
	if (symbol.isClass()) {
	    // type parameters
	    Symbol[] tparams = symbol.typeParams();
	    if (tparams.length != 0 || global.debug) {
		print('[');
		for (int i = 0; i < tparams.length; i++) {
		    if (i > 0) print(",");
		    printSignature(tparams[i], user, false);
		}
		print(']');
	    }
	    // value parameters
	    Symbol[] vparams = symbol.valueParams();
	    print('(');
	    for (int i = 0; i < vparams.length; i++) {
		if (i > 0) print(", "); // vincent
		if (vparams[i].isDefParameter()) print("def ");
		htmlGenerator.defString(vparams[i], user, false);
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
    public SymbolTablePrinter printTemplateHtmlSignature(Symbol symbol, Symbol user, boolean addLink) {
	// modifiers
        String mods = Modifiers.Helper.toString(symbol.flags);
	page.printlnOTag("dl");
	page.printlnOTag("dt");
	print(mods).space();

        // kind
	String keyword = getSymbolKeyword(symbol);
        if (keyword != null) print(keyword).space();
        String inner = getSymbolInnerString(symbol);

        // name
	printDefinedSymbolName(symbol, user, addLink);
	if (symbol.isClass()) {
	    // type parameters
	    Symbol[] tparams = symbol.typeParams();
	    if (tparams.length != 0 || global.debug) {
		print('[');
		for (int i = 0; i < tparams.length; i++) {
		    if (i > 0) print(",");
		    printSignature(tparams[i], user, false);
		}
		print(']');
	    }
	    // value parameters
	    Symbol[] vparams = symbol.valueParams();
	    print('(');
	    for (int i = 0; i < vparams.length; i++) {
		if (i > 0) print(", ");
		if (vparams[i].isDefParameter()) print("def ");
		htmlGenerator.defString(vparams[i], user, false);
	    }
	    print(')');
	}

        // parents
        Type[] parts = symbol.moduleClass().parents();
        page.printlnCTag("dt");
        for (int i = 0; i < parts.length; i++) {
            page.printOTag("dd");
            print((i == 0) ? "extends " : "with ");
            printType(parts[i], symbol);
	    page.printlnCTag("dd");
	}
	page.printCTag("dl");
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
    public SymbolTablePrinter printTypes(Type[] types, String infix, Symbol user) {
        for (int i = 0; i < types.length; i++) {
            if (i > 0) print(infix);
            printType(types[i], user);
        }
        return this;
    }

    /**
     * Prints the given type.
     *
     * @param type
     * @param user The symbol using the type <code>type</code>
     */
    public SymbolTablePrinter printType(Type type, Symbol user) {
        return printType0(getTypeToPrintForType(type), user);
    }

    /**
     * ..
     *
     * @param type
     * @param user
     */
    public SymbolTablePrinter printType0(Type type, Symbol user) {
        printCommonPart(type, user);
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
    public SymbolTablePrinter printType(Type type, String inner, Symbol user) {
	if ("<:".equals(inner) && type.symbol() == global.definitions.ANY_CLASS ||
	    ">:".equals(inner) && type.symbol() == global.definitions.ALL_CLASS)
	    return this;
        else
	    return printType0(getTypeToPrintForType(type), inner, user);
    }

    /**
     * ..
     *
     * @param type
     * @param inner
     */
    public SymbolTablePrinter printType0(Type type, String inner, Symbol user) {
        switch (type) {
        case MethodType(Symbol[] vparams, Type result):
            print('(');
            for (int i = 0; i < vparams.length; i++) {
                if (i > 0) print(", ");
		if (vparams[i].isDefParameter()) print("def ");
		htmlGenerator.defString(vparams[i], user, false);
		//print(NameTransformer.decode(vparams[i].name.toString()) + ": ");// vincent
                //printSymbolType(vparams[i], null);
            }
            print(')');
            return printType(result, inner, user);
        case PolyType(Symbol[] tparams, Type result):
            if (tparams.length != 0 || global.debug) {
                print('[');
                for (int i = 0; i < tparams.length; i++) {
                    if (i > 0) print(",");
                    printSignature(tparams[i], user, false);
                }
                print(']');
            }
            return printType(result, inner, user);
        default:
            if (inner != null) {
                if (!inner.startsWith(":")) space();
                print(inner).space();
            }
            return printType0(type, user);
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
        printTypes(args, ", ", Symbol.NONE);
        print(") => ");
        printType(types[types.length - 1], Symbol.NONE);
        return this;
    }

    /**
     * Prints a template type with the given base types.
     *
     * @param types
     */
    public SymbolTablePrinter printTemplateType(Type[] types) {
        print("<template: ");
        printTypes(types, " with ", Symbol.NONE);
        print(" {...}>");
        return this;
    }

    /**
     * Prints the type and prefix common part of the given type.
     *
     * @param type
     * @param user The symbol using the type <code>type</code>
     */
    public SymbolTablePrinter printCommonPart(Type type, Symbol user) {
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
            printUsedSymbolName(sym, user);
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
            printUsedSymbolName(sym, user);
	    if (args.length != 0) {
                print('[');
                printTypes(args, ",", user);
                print(']');
            }
            return this;

	case SingleType(Type pre, Symbol sym):
            printPrefix(pre, sym);
            printUsedSymbolName(sym, user);
            return this;

	case ConstantType(Type base, Object value):
	    System.out.println("CONSTANT" + type);
	    print("(");
	    printType(base);
	    print(value.toString());
	    print(")");
	    return this;

	case CompoundType(Type[] parts, Scope members):
	    printTypes(parts," with ", user);
            space();
            return printScope(members, true); // vincent

	case MethodType(_, _):
	    return printType0(type, user);

	case PolyType(_, _):
	    return printType0(type, user);

	case OverloadedType(Symbol[] alts, Type[] alttypes):
            return printTypes(alttypes, " <and> ", user);

	case TypeVar(Type origin, Constraint constr):
            printType(origin, user);
            print("?");
            return this;

	case UnboxedType(int kind):
	    print(type.unboxedName(kind).toString());
            return this;

	case UnboxedArrayType(Type elemtp):
	    printType(elemtp, user);
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
    public Type getTypeToPrintForPrefix0(Ty*pe prefix) {
        if (!global.debug) {
            if (prefix.symbol().kind == Kinds.NONE) return null;
            if (prefix.symbol().isRoot()) return null;
        }
        return getTypeToPrintForType0(prefix);
    }

    /**
     * ..
     */
    public SymbolTablePrinter printPrefix0(Type prefix) {
        printCommonPart(prefix, Symbol.NONE);
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
