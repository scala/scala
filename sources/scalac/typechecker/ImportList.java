/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**
** $Id$
\*                                                                      */

package scalac.typechecker;

import scalac.*;
import scalac.util.*;
import scalac.symtab.*;
import scalac.ast.*;

/////////////////////////////////////////////////////////////////////////////
// Import Lists
/////////////////////////////////////////////////////////////////////////////

class ImportList {
    Tree tree;           // The import definition
    Scope enclscope;         // The scope in which the import occurs.
    ImportList prev;     // The previous active import list.

    ImportList(Tree tree, Scope enclscope, ImportList prev) {
	this.tree = tree;
	this.enclscope = enclscope;
	this.prev = prev;
    }

    public String toString() {
	String str = tree.symbol().toString();
	if (prev != null) str = prev + "; " + str;
	return str;
    }

    Tree importPrefix() {
	switch (tree) {
	case Import(Tree expr, _): return expr;
	default: throw new ApplicationError();
	}
    }

    Type importType() {
	return tree.symbol().type();
    }

    boolean sameImport(ImportList that) {
	return this.importType().isSameAs(that.importType());
    }

    Symbol importedSymbol(Name name) {
	Type t = this.importType();
	boolean renamed = false;
	switch (tree) {
	case Import(Tree expr, Name[] selectors):
	    for (int i = 0; i < selectors.length; i = i + 2) {
		if (i + 1 < selectors.length && name.toTermName() == selectors[i + 1]) {
		    if (name.isTypeName())
			return t.lookupNonPrivate(selectors[i].toTypeName());
		    else
			return t.lookupNonPrivate(selectors[i]);
		} else if (name.toTermName() == selectors[i]) {
		    renamed = true;
		} else if (selectors[i] == Names.IMPORT_WILDCARD && !renamed) {
		    return t.lookupNonPrivate(name);
		}
	    }
	    return Symbol.NONE;
	default:
	    throw new ApplicationError();
	}
    }
}
