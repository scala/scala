/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.transformer;

import scalac.Global;
import scalac.util.Name;
import scalac.util.Names;

import scalac.ast.Tree;
import Tree.*;
import scalac.ast.Transformer;

import scalac.symtab.Type;
import scalac.symtab.Symbol;
import scalac.symtab.SymbolSubstTypeMap;
import scalac.symtab.TermSymbol;
import scalac.symtab.Scope;
import scalac.symtab.Kinds;
import scalac.symtab.Modifiers;

import java.util.HashMap;
import java.util.HashSet;
import java.util.ArrayList;

import scalac.util.Debug;

/**
 * 1. For each class add a single method (DefDef) designated as
 * constructor whose parameters are the class parameters and the
 * owner is the constructed class. The constructor contains:
 *
 *   - call to the constructor of the supertype with the appropriate arguments
 *   - initialize the fields corresponding to the class ValDefs
 *   - the bodies of the class level expressions;
 *     they are also removed from the ClassDef body
 *
 * 2. Additional constructors are assigned new symbols owned by the class
 *
 * 3. The right hand sides of the ValDefs are left empty
 *
 * 4. Substitute the appropriate constructor symbol into the
 * 'new' expressions and constructor applications
 *
 * @author Nikolay Mihaylov
 * @version 1.2
*/

public class AddConstructors extends Transformer {

    // True iff we generate code for INT backend.
    protected final boolean forINT;

    protected final HashMap/*<Symbol, Symbol>*/ constructors;

    private final SymbolSubstTypeMap subst;

    public AddConstructors(Global global, HashMap constructors) {
	super(global);
        this.constructors = constructors;
        this.forINT = global.target == global.TARGET_INT;
        this.subst = new SymbolSubstTypeMap();
    }

    /** return new constructor symbol if it isn't already defined
     */
    Symbol getConstructor(Symbol classConstr) {
   	assert classConstr.isConstructor() :
   	    "Class constructor expected: " + Debug.show(classConstr);
        Symbol owner = classConstr.constructorClass();
        Symbol[] tparamSyms = classConstr.typeParams();
        Symbol[] paramSyms = classConstr.valueParams();
	Symbol constr = (Symbol) constructors.get(classConstr);
	if (constr == null) {
	    assert !owner.isInterface() : Debug.show(owner) + " is interface";
            int flags =
                classConstr.flags & (Modifiers.PRIVATE | Modifiers.PROTECTED);
	    constr =
                new TermSymbol(classConstr.pos, classConstr.name, owner, flags);

	    Type constrType = Type.MethodType
		(paramSyms, forINT ? owner.type()
		 : global.definitions.UNIT_TYPE());
            if (tparamSyms.length != 0)
                constrType = Type.PolyType(tparamSyms, constrType);

            if (!classConstr.isExternal())
                constrType.cloneType(classConstr, constr);

	    constr.setInfo(constrType);
	    constructors.put(classConstr, constr);
	    constructors.put(constr, constr);
            owner.members().enterOrOverload(constr);
	}

	return constr;
    }

    /** process the tree
     */
    public Tree transform(Tree tree) {
	final Symbol treeSym = tree.symbol();
	switch (tree) {
	case ClassDef(_, _, _, _, _, Template impl):
	    if (treeSym.isInterface())
		return gen.ClassDef(treeSym, transform(impl.body));

	    // expressions that go before the call to the super constructor
	    final ArrayList constrBody = new ArrayList();

	    // expressions that go after the call to the super constructor
	    final ArrayList constrBody2 = new ArrayList();

	    // the body of the class after the transformation
	    final ArrayList classBody = new ArrayList();

	    // the Symbol of the new primary constructor
	    final Symbol constrSym =
		getConstructor(treeSym.primaryConstructor());

	    assert constrSym.owner() == treeSym :
		"Wrong owner of the constructor: \n\tfound: " +
		Debug.show(constrSym.owner()) + "\n\texpected: " +
		Debug.show(treeSym);

	    for (int i = 0; i < impl.body.length; i++) {
		Tree t = impl.body[i];
		if (t.definesSymbol()) {
		    Symbol sym = t.symbol();
		    switch (t) {
		    // for ValDefs move the initialization code to the constructor
		    case ValDef(_, _, _, Tree rhs):
			if (rhs != Tree.Empty) {
			    Tree lhs =
			        gen.Select(gen.This(t.pos, treeSym), sym);
			    Tree assign =
				gen.Assign(t.pos, lhs, rhs);
			    t = gen.ValDef(sym, Tree.Empty);
			    if (rhs.hasSymbol() && rhs.symbol().isParameter()) {
				constrBody.add(assign);
			    } else {
				constrBody2.add(assign);
			    }
			}
			break;

		    }
		    // do not transform(t). It will be done at the end
		    classBody.add(t);
		} else {
		    // move class-level expressions into the constructor
		    constrBody2.add(impl.body[i]);
		}
	    }

	    // inline the call to the super constructor
            if ( !forINT || !treeSym.parents()[0].symbol().isJava()) {
		switch (impl.parents[0]) {
		case Apply(TypeApply(Tree fun, Tree[] targs), Tree[] args):
                    assert fun.symbol().isConstructor(): impl.parents[0];
		    int pos = impl.parents[0].pos;
		    Tree superConstr = gen.Select
			(gen.Super(pos, treeSym), fun.symbol());
		    constrBody.add(gen.mkApplyTV(superConstr, targs, args));
		    break;
		case Apply(Tree fun, Tree[] args):
                    assert fun.symbol().isConstructor(): impl.parents[0];
		    int pos = impl.parents[0].pos;
		    Tree superConstr = gen.Select
			(gen.Super(pos, treeSym), fun.symbol());
		    constrBody.add(gen.mkApply_V(superConstr, args));
		    break;
		default:
                    throw Debug.abort("illegal case", impl.parents[0]);
		}
	    } else {
                constrBody.add(gen.This(tree.pos, treeSym));
            }

	    // inline initialization of module values
            if (forINT && treeSym.isModuleClass()) {
                Symbol module = treeSym.module();
                if (module.isGlobalModule()) {
                    constrBody.add(
                        gen.Assign(
                            gen.mkRef(tree.pos, module),
                            gen.This(tree.pos, treeSym)));
                } else {
                    Symbol owner = module.owner();
                    Name module_eqname = module.name.append(Names._EQ);
                    Symbol module_eq = owner.lookup(module_eqname);
                    assert module != Symbol.NONE :Debug.show(treeSym.module());
                    if (owner.isModuleClass() && owner.module().isStable()) {
                        constrBody.add(
                            gen.mkApply_V(
                                gen.mkRef(tree.pos, module_eq),
                                new Tree[] {gen.This(tree.pos, treeSym)}));
                    } else {
                        // !!! module_eq must be accessed via some outer field
                    }
                }
            }

	    // add valdefs and class-level expression to the constructorr body
	    constrBody.addAll(constrBody2);

            Tree constrTree = constrBody.size() > 1 ?
                gen.Block((Tree[])constrBody.
			  toArray(new Tree[constrBody.size()])):
                (Tree) constrBody.get(0);

	    classBody.add(gen.DefDef(treeSym.primaryConstructor(),constrTree));

	    Tree[] newBody = (Tree[]) classBody.toArray(Tree.EMPTY_ARRAY);

	    // transform the bodies of all members in order to substitute
	    // the constructor references with the new ones
	    return gen.ClassDef(treeSym, transform(newBody));

        case DefDef(_, _, _, _, _, Tree rhs):
            // assign new symbols to the alternative constructors
            Symbol constr = tree.symbol();
            if (!constr.isConstructor()) break;
            // add result expression consistent with the
            // result type of the constructor
            Symbol init = getConstructor(constr);
            subst.insertSymbol(constr.typeParams(), init.typeParams());
            subst.insertSymbol(constr.valueParams(), init.valueParams());
            rhs = transform(rhs);
            subst.removeSymbol(constr.valueParams());
            subst.removeSymbol(constr.typeParams());
            Tree result = forINT
                ? gen.This(rhs.pos, constr.constructorClass())
                : gen.mkUnitLit(rhs.pos);
            rhs = gen.mkBlock(new Tree[] { rhs, result });
            return gen.DefDef(init, rhs);

	// Substitute the constructor into the 'new' expressions
	case New(Template(Tree[] baseClasses, _)):
	    Tree base = baseClasses[0];
	    switch (base) {
	    case Apply(TypeApply(Tree fun, Tree[] targs), Tree[] args):
                return gen.New(
                    tree.pos,
                    gen.mkApplyTV(
                        base.pos,
                        gen.Ident(fun.pos, getConstructor(fun.symbol())),
                        transform(targs),
                        transform(args)));
	    case Apply(Tree fun, Tree[] args):
                return gen.New(
                    tree.pos,
                    gen.mkApply_V(
                        base.pos,
                        gen.Ident(fun.pos, getConstructor(fun.symbol())),
                        transform(args)));
	    default:
		assert false;
	    }
	    break;

	// Substitute the constructor aplication;
	// this only occurs within constructors that terminate
	// by a call to another constructor of the same class
	case Apply(TypeApply(Tree fun, Tree[] targs), Tree[] args):
            if (!fun.symbol().isConstructor()) break;
	    Symbol constr = getConstructor(fun.symbol());
            fun = gen.Select(fun.pos, getInitializerQualifier(fun), constr);
            return gen.mkApplyTV(tree.pos, fun, transform(targs), transform(args));

	case Apply(Tree fun, Tree[] args):
            if (!fun.symbol().isConstructor()) break;
	    Symbol constr = getConstructor(fun.symbol());
            fun = gen.Select(fun.pos, getInitializerQualifier(fun), constr);
            return gen.mkApply_V(tree.pos, fun, transform(args));

        case Ident(_):
            Symbol symbol = subst.lookupSymbol(tree.symbol());
            if (symbol == null) break;
            return gen.Ident(tree.pos, symbol);

        case TypeTerm():
            return gen.mkType(tree.pos, subst.apply(tree.type()));

	} // switch(tree)

	return super.transform(tree);
    } // transform()

    private Tree getInitializerQualifier(Tree tree) {
        switch (tree) {
        case Select(Tree qualifier, _):
            return qualifier;
        case Ident(_):
            return gen.This(tree.pos, tree.symbol().constructorClass());
        default:
            throw Debug.abort("illegal case", tree);
        }
    }

} // class AddConstructors
