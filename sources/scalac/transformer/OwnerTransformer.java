/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.transformer;

import java.io.*;
import java.util.*;
import scalac.*;
import scalac.util.*;
import scalac.ast.*;
import scalac.symtab.*;
import Tree.*;


/** A default transformer class which also maintains owner information
 *
 *  @author     Martin Odersky
 *  @version    1.0
 */
public class OwnerTransformer extends Transformer {

    protected Symbol currentOwner;

    public OwnerTransformer(Global global) {
        super(global);
    }

    public void apply(Unit unit) {
	currentOwner = global.definitions.ROOT_CLASS;
        unit.body = transform(unit.body);
    }

    public Tree transform(Tree tree, Symbol owner) {
	Symbol prevOwner = currentOwner;
	currentOwner = owner;
	Tree tree1 = transform(tree);
	currentOwner = prevOwner;
	return tree1;
    }

    public TypeDef[] transform(TypeDef[] params, Symbol owner) {
	Symbol prevOwner = currentOwner;
	currentOwner = owner;
	TypeDef[] res = transform(params);
	currentOwner = prevOwner;
	return res;
    }

    public ValDef[][] transform(ValDef[][] params, Symbol owner) {
	Symbol prevOwner = currentOwner;
	currentOwner = owner;
	ValDef[][] res = transform(params);
	currentOwner = prevOwner;
	return res;
    }

    public Template transform(Template templ, Symbol owner) {
	Symbol prevOwner = currentOwner;
	if (owner.kind == Kinds.CLASS)
	    currentOwner = owner.constructor();
	Tree[] parents1 = transform(templ.parents);
	currentOwner = owner;
	Tree[] body1 = transformTemplateStats(templ.body, templ.symbol());
	currentOwner = prevOwner;
	return copy.Template(templ, parents1, body1);
    }

    public Tree[] transformTemplateStats(Tree[] ts, Symbol tsym) {
	Tree[] ts1 = ts;
	for (int i = 0; i < ts.length; i++) {
            Tree t = transformTemplateStat(ts[i], tsym);
            if (t != ts[i] && ts1 == ts) {
                ts1 = new Tree[ts.length];
                System.arraycopy(ts, 0, ts1, 0, i);
	    }
	    ts1[i] = t;
        }
        return ts1;
    }

    public Tree transformTemplateStat(Tree stat, Symbol tsym) {
	return transform(stat, tsym);
    }

    public Tree transform(Tree tree) {
	switch(tree) {
	case PackageDef(Tree packaged, Template impl):
	    return copy.PackageDef(
		tree,
                transform(packaged),
                transform(impl, packaged.symbol()));

	case ClassDef(_, _, TypeDef[] tparams, ValDef[][] vparams, Tree tpe, Template impl):
            Symbol symbol = tree.symbol();
	    return copy.ClassDef(
		tree, symbol,
		transform(tparams, symbol.constructor()),
		transform(vparams, symbol.constructor()),
		transform(tpe),
		transform(impl, symbol));

	case ModuleDef(_, _, Tree tpe, Template impl):
            Symbol symbol = tree.symbol();
	    return copy.ModuleDef(
		tree, symbol,
                transform(tpe),
		transform(impl, symbol.moduleClass()));

	case DefDef(_, _, TypeDef[] tparams, ValDef[][] vparams, Tree tpe, Tree rhs):
            Symbol symbol = tree.symbol();
	    return copy.DefDef(
		tree, symbol,
		transform(tparams, symbol),
		transform(vparams, symbol),
		transform(tpe, symbol),
		transform(rhs, symbol));

	case ValDef(_, _, Tree tpe, Tree rhs):
            Symbol symbol = tree.symbol();
	    return copy.ValDef(
		tree, symbol,
                transform(tpe),
		transform(rhs, symbol));

	case TypeDef(_, _, Tree rhs):
            Symbol symbol = tree.symbol();
	    return copy.TypeDef(
		tree, symbol,
                transform(rhs, symbol));

	default:
	    return super.transform(tree);
	}
    }
}
