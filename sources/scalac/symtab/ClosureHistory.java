/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.symtab;

import java.util.Iterator;
import java.util.TreeMap;

import scalac.Global;
import scalac.framework.History;
import scalac.util.Debug;
import scalac.util.ArrayApply;

/** This class implements a closure history. */
public class ClosureHistory extends History {

    //########################################################################
    // Protected Methods

    /** Transforms given closure into closure at next phase. */
    protected Object transformValue(Object owner, Object value){
        Type[] closure = (Type[])value;
        for (int i = 0; i < closure.length; i++) {
            Symbol symbol = closure[i].symbol();
            // !!! symbol.nextInfoHasChanged() would be better
            if (symbol.info() != symbol.nextInfo())
                return super.transformValue(owner, closure);
        }
        return closure;
    }

    /** Computes the closure at current phase. */
    protected Object computeValue(Object owner) {
        Symbol clasz = (Symbol)owner;
        TreeMap parents = new TreeMap(SymbolComparator.instance);
        addParents(parents, clasz.info());
        Type[] closure = new Type[1 + parents.size()];
        Iterator types = parents.values().iterator();
        closure[0] = clasz.type();
        for (int i = 1; i < closure.length; i++)
            closure[i] = (Type)types.next();
	//System.out.println("closure(" + owner + ") = " + ArrayApply.toString(closure));//DEBUG
        return closure;
    }

    //########################################################################
    // Private Functions

    /** Adds all parents of given type to given parent table. */
    private static void addParents(TreeMap/*<Symbol,Type>*/ table, Type type) {
        switch (type) {
        case ErrorType: case NoType:
            return;
        case TypeRef(_, Symbol symbol, _):
            Type.Map map = Type.getThisTypeMap(symbol, type);
            Type[] closure = symbol.closure();
            for (int i = 0; i < closure.length; i++)
                table.put(closure[i].symbol(), map.apply(closure[i]));
            return;
        case CompoundType(Type[] parents, _):
            for (int i = 0; i < parents.length; i++)
                addParents(table, parents[i]);
            return;
        default:
            throw Debug.abort("illegal case", type);
        }
    }

    //########################################################################
}
