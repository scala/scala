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
		tree, transform(packaged), transform(impl, packaged.symbol()));

	case ClassDef(int mods, Name name, TypeDef[] tparams, ValDef[][] vparams, Tree tpe, Template impl):
	    return copy.ClassDef(
		tree, mods, name,
		transform(tparams, tree.symbol().constructor()),
		transform(vparams, tree.symbol().constructor()),
		transform(tpe),
		transform(impl, tree.symbol()));

	case ModuleDef(int mods, Name name, Tree tpe, Template impl):
	    return copy.ModuleDef(
		tree, mods, name, transform(tpe),
		transform(impl, tree.symbol().moduleClass()));

	case DefDef(int mods, Name name, TypeDef[] tparams, ValDef[][] vparams, Tree tpe, Tree rhs):
	    return copy.DefDef(
		tree, mods, name,
		transform(tparams, tree.symbol()),
		transform(vparams, tree.symbol()),
		transform(tpe, tree.symbol()),
		transform(rhs, tree.symbol()));

	case ValDef(int mods, Name name, Tree tpe, Tree rhs):
	    return copy.ValDef(
		tree, mods, name, transform(tpe),
		transform(rhs, tree.symbol()));

	case TypeDef(int mods, Name name, Tree rhs):
	    return copy.TypeDef(
		tree, mods, name, transform(rhs, tree.symbol()));

	default:
	    return super.transform(tree);
	}
    }
}
