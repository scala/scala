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
import scalac.ast.GenTransformer;

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
 * This phase adds to all classes one initializer method per
 * constructor. Initializers have the same value parameters as their
 * corresponding constructor but no type parameters.
 *
 * An initializer may be used in the body of another initializer
 * either to invoke another initializer of the same class or to invoke
 * an initailizer of the superclass. In that case, the initializer
 * must appear in a Select node whose qualifier is either a This node
 * or a Super node. The type of such a Select node is the type of the
 * initializer.
 *
 * An initializer may also be used in a new operation. In that case,
 * the initializer must appear in an Ident node.  The type of such an
 * Ident node is a PolyType whose arguments are the type arguments of
 * the initializer's class and whose result type is the initializer's
 * type.
 *
 * This phase does the following in the tree:
 *
 * - replaces all non-primary constructors definitions by initializer
 *   definitions with the same body,
 *
 * - for each non-interface class creates an primary initializer
 *   method corresponding to the primary constructor,
 *
 * - moves the call to the super constructor into the primary
 *   initializer,
 *
 * - moves all class fields initialization code (rhs of ValDefs) into
 *   the primary initializer,
 *
 * - moves all class-level expressions into the primary initializer,
 *
 * - replaces all constructor invocations by initializer invocations,
 *   except in the parents field of class templates.
 *
 * @author Nikolay Mihaylov
 * @version 1.2
 */
public class AddConstructors extends GenTransformer {

    /** True iff we generate code for INT backend. */
    private final boolean forINT;

    /** A constructor to initializer map */
    private final HashMap/*<Symbol,Symbol>*/ initializers;

    /** A constructor to initializer parameter substitution */
    private final SymbolSubstTypeMap subst;

    public AddConstructors(Global global, HashMap initializers) {
	super(global);
        this.initializers = initializers;
        this.forINT = global.target == global.TARGET_INT;
        this.subst = new SymbolSubstTypeMap();
    }

    /** Returns the initializer corresponding to the given constructor. */
    private Symbol getInitializer(Symbol constructor) {
   	assert constructor.isConstructor(): Debug.show(constructor);
	Symbol initializer = (Symbol)initializers.get(constructor);
	if (initializer == null) {
	    assert !constructor.constructorClass().isInterface():
                "found interface constructor " + Debug.show(constructor);
	    initializer = new TermSymbol(
                constructor.pos,
                constructor.name,
                constructor.constructorClass(),
                constructor.flags & Modifiers.ACCESSFLAGS);
	    initializer.setInfo(
                Type.MethodType(
                    constructor.valueParams(),
                    global.definitions.UNIT_TYPE())
                .cloneType(constructor, initializer));
            initializer.owner().members().enterOrOverload(initializer);
	    initializers.put(constructor, initializer);
	}
	return initializer;
    }

    /** process the tree
     */
    public Tree transform(Tree tree) {
        return transform(tree, false);
    }

    private Tree transform(Tree tree, boolean inNew) {
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
            switch (impl.parents[0]) {
            case Apply(TypeApply(Tree fun, Tree[] targs), Tree[] args):
                assert fun.symbol().isConstructor(): impl.parents[0];
                int pos = impl.parents[0].pos;
                Tree superConstr = gen.Select
                    (gen.Super(pos, treeSym), getInitializer(fun.symbol()));
                constrBody.add(gen.mkApply_V(superConstr, args));
                break;
            case Apply(Tree fun, Tree[] args):
                assert fun.symbol().isConstructor(): impl.parents[0];
                int pos = impl.parents[0].pos;
                Tree superConstr = gen.Select
                    (gen.Super(pos, treeSym), getInitializer(fun.symbol()));
                constrBody.add(gen.mkApply_V(superConstr, args));
                break;
            default:
                throw Debug.abort("illegal case", impl.parents[0]);
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
            if (!tree.symbol().isConstructor()) return super.transform(tree);
            // replace constructor by initializer
            Symbol constructor = tree.symbol();
            Symbol initializer = getInitializer(constructor);
            subst.insertSymbol(
                constructor.typeParams(),
                constructor.constructorClass().typeParams());
            subst.insertSymbol(
                constructor.valueParams(),
                initializer.valueParams());
            rhs = transform(rhs);
            subst.removeSymbol(constructor.valueParams());
            subst.removeSymbol(constructor.typeParams());
            // add consistent result expression
            rhs = gen.mkBlock(new Tree[] { rhs, gen.mkUnitLit(rhs.pos) });
            return gen.DefDef(initializer, rhs);

        case New(Template(Tree[] parents, _)):
            return gen.New(transform(parents[0], true));

 	case TypeApply(Tree fun, Tree[] args):
            if (!inNew && fun.symbol().isConstructor()) return transform(fun);
            return gen.TypeApply(transform(fun, inNew), transform(args));

 	case Apply(Tree fun, Tree[] args):
            return gen.Apply(transform(fun, inNew), transform(args));

        case Ident(_):
            Symbol symbol = tree.symbol();
            if (symbol.isConstructor()) {
                symbol = getInitializer(symbol);
                if (!inNew) {
                    Symbol clasz = symbol.owner();
                    return gen.Select(gen.This(tree.pos, clasz), symbol);
                }
            } else if (symbol.owner().isConstructor()) {
                symbol = subst.lookupSymbol(symbol);
            }
            return gen.Ident(tree.pos, symbol);

        case TypeTerm():
            return gen.TypeTerm(tree.pos, subst.apply(tree.getType()));

        default:
            return super.transform(tree);
	} // switch(tree)

    } // transform()

} // class AddConstructors
