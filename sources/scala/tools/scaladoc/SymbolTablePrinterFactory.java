/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.scaladoc;

import scalac.Global;
import scalac.symtab.Kinds;
import scalac.symtab.Modifiers;
import scalac.symtab.NoSymbol;
import scalac.symtab.Scope;
import scalac.symtab.Scope.SymbolIterator;
import scalac.symtab.Symbol;
import scalac.symtab.Type;
import scalac.symtab.Type.*;
//import scalac.symtab.SymbolTablePrinter;
import scalac.util.Debug;
import scalac.util.Name;
import scalac.util.Names;
import scalac.util.Strings;
import SymbolBooleanFunction;

class SymbolTablePrinterFactory {

    public static SymbolTablePrinter makeHTML(final Page page, final SymbolBooleanFunction isDocumented) {

	return new SymbolTablePrinter(page.getCodePrinter()) {

		public void printSymbol(Symbol sym, boolean addLink) {
		    String name = sym.nameString();
		    if (global.debug) name = sym.name.toString();
		    if (isDocumented.apply(sym))
			if (addLink)
			    page.printAhref(page.rel(Location.get(sym)),
					    page.destinationFrame, name);
			else {
			    page.printOTag("em");
			    page.print(name);
			    page.printCTag("em");
			}
		    else
			page.print(name);
		}

		public Type getTypeToPrintForPrefix(Type prefix/*, sym*/) {
		    while (true) {
			Type result = getTypeToPrintForPrefix0(prefix);
			if (result == prefix || result == null) {
			    return
				cleanPrefix(result, global, isDocumented);
			}
			prefix = result;
		    }
		}

		protected String INNER_LT = "&lt;:";

		public String getSymbolInnerString(Symbol symbol) {
		    if (symbol.kind == Kinds.TYPE)
			return INNER_LT; // HTML encoded "<:" symbol
		    else
			return super.getSymbolInnerString(symbol);
		}

		public SymbolTablePrinter printType(Type type, String inner) {
		    if ((INNER_LT.equals(inner) && type.symbol() == global.definitions.ANY_CLASS) ||
			(">:".equals(inner) && type.symbol() == global.definitions.ALL_CLASS))
			return this;
		    else {
			printType0(getTypeToPrintForType(type), inner);
			return this;
		    }
		}
	    };
    }

    /**
     * Removes the longest prefix of this type which corresponds to a
     * nested of class and object definitions. The idea is that this
     * prefix is redondant and can be recovered directly from the tree
     * itself.
     *
     * @param prefix
     */
    static protected Type cleanPrefix(Type prefix, Global global, SymbolBooleanFunction isDocumented) {
	if (prefix == null) return null;
	if (prefix.symbol().kind == Kinds.NONE) return null;
	if (prefix.symbol().isRoot()) return null;

	// Next line should be removed in theory.
        if (prefix.symbol().moduleClass() ==
	    global.definitions.getClass(Names.java_lang))
            return null;

	switch(prefix) {
	case ThisType(Symbol sym):
	    if (sym.isPackage() && isDocumented.apply(sym.module()))
		return null;
	    else if (isDocumented.apply(sym))
		return null;
	    else
		return prefix;
	case TypeRef(Type pre, Symbol sym, Type[] args):
	    Type pre1 = cleanPrefix(pre, global, isDocumented);
	    if (pre1 == null && args.length == 0 && isDocumented.apply(sym))
		return null;
	    else {
		pre1 = pre1 == null ? global.definitions.ROOT.thisType() : pre1;
		return Type.typeRef(pre1, sym, args);
	    }
	case SingleType(Type pre, Symbol sym):
	    Type pre1 = cleanPrefix(pre, global, isDocumented);
	    if (pre1 == null) {
		if (sym.isClass() || sym.isModule())
		    if (isDocumented.apply(sym)) {
			return null;
		    }
		    else
			return Type.singleType(global.definitions.ROOT.thisType(), sym);
		else
		    return Type.singleType(global.definitions.ROOT.thisType(), sym);
	    }
	    else
		return Type.singleType(pre1, sym);
	default:
	    return prefix;
	}
    }



}
