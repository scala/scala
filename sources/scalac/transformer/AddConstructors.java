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
 * constructor whose parameters are the class parameters. The
 * constructor contains:
 *   - call to the constructor of the supertype with the appropriate arguments
 *   - initialize the fields corresponding to the class ValDefs
 *   - the bodies of the class level expressions;
 *     they are also removed from the ClassDef body
 *
 * 2. The right hand sides of the ValDefs are left empty
 *
 * 3. Substitute the appropriate constructor symbol into the
 * 'new' expressions
 *
 * @author Nikolay Mihaylov
 * @version 1.2
*/

public class AddConstructors extends Transformer {
    public final static Name CTOR_N = Names.CONSTRUCTOR;

    // True iff we generate code for INT backend.
    protected final boolean forINT;
    // True iff we generate code for JVM backend.
    protected final boolean forJVM;
    // True iff we generate code for MSIL backend.
    protected final boolean forMSIL;

    final HashMap constructors;

    public AddConstructors(Global global, HashMap constructors) {
	super(global);
        this.constructors = constructors;
        this.forINT = global.target == global.TARGET_INT;
        this.forJVM = (global.target == global.TARGET_JVM
                       || global.target == global.TARGET_JVM_BCEL);
        this.forMSIL = global.target == global.TARGET_MSIL;
    }

    /** return new constructor symbol if it isn't already defined
     */
    Symbol getConstructor(Symbol classConstr) {
	return getConstructor(classConstr,
			      (((Type.MethodType)classConstr.info()).vparams),
			      classConstr.constructorClass());
    }

    Symbol getConstructor(Symbol classConstr, Symbol[] paramSyms, Symbol owner) {
  	assert classConstr.isConstructor() :
  	    "Class constructor expected: " + Debug.show(classConstr);

	Symbol constr = (Symbol) constructors.get(classConstr);
	if (constr == null) {
	    assert !owner.isInterface() : Debug.show(owner) + " is interface";
            int flags = forJVM
                ? classConstr.flags & (Modifiers.PRIVATE | Modifiers.PROTECTED)
                : classConstr.flags;
	    constr =
                new TermSymbol(classConstr.pos, CTOR_N, owner, flags);

	    Type constrType = Type.MethodType
		(paramSyms, forJVM ?
		 global.definitions.UNIT_TYPE : owner.type()).erasure();

	    constr.setInfo(constrType.erasure());
	    constructors.put(classConstr, constr);
	    constructors.put(constr, constr);
	}

	return constr;
    }

    /** process the tree
     */
    public Tree transform(Tree tree) {
	Symbol treeSym = tree.symbol();
	switch (tree) {
	case ClassDef(_, _, _, ValDef[][] vparams, _, //:
		      Template(Tree[] baseClasses, Tree[] body)):

	    assert treeSym.name.isTypeName();

	    if (treeSym.isInterface())
		return super.transform(tree);

	    Symbol[] paramSyms = new Symbol[vparams[0].length];
	    for (int i = 0; i < paramSyms.length; i++)
		paramSyms[i] = vparams[0][i].symbol();

	    ArrayList constrBody = new ArrayList();
	    ArrayList constrBody2 = new ArrayList();
	    ArrayList classBody = new ArrayList();
	    Symbol constrSym =
		getConstructor(treeSym.primaryConstructor(), paramSyms, treeSym);
	    Scope classScope = new Scope();
	    classScope.enter(constrSym);

	    assert constrSym.owner() == treeSym :
		"Wrong owner of the constructor: \n\tfound: " +
		Debug.show(constrSym.owner()) + "\n\texpected: " +
		Debug.show(treeSym);

	    // for every ValDef move the initialization code into the constructor
	    for (int i = 0; i < body.length; i++) {
		Tree t = body[i];
		if (t.definesSymbol()) {
		    Symbol sym = t.symbol();
		    switch (t) {
		    case ValDef(_, _, _, Tree rhs):
			if (rhs != Tree.Empty) {
			    Tree lhs =
			        gen.Select(gen.This(t.pos, treeSym), sym);
			    Tree assign =
				gen.Assign(t.pos, lhs, transform(rhs));
			    t = gen.ValDef(sym, Tree.Empty);
			    if (rhs.hasSymbol() && rhs.symbol().isParameter()) {
				constrBody.add(assign);
			    } else {
				constrBody2.add(assign);
			    }
			}
			break;
		    }
		    classBody.add(transform(t));
		    classScope.enterOrOverload(sym);
		} else {
		    // move class-level expressions into the constructor
		    constrBody2.add(transform(body[i]));
		}
	    }

	    // inline the call to the super constructor
            if ( !forINT || !treeSym.parents()[0].symbol().isJava()) {
		switch (baseClasses[0]) {
		case Apply(Tree fun, Tree[] args):
		    int pos = baseClasses[0].pos;
		    Tree superConstr = gen.Select
			(gen.Super(pos, treeSym),
			 getConstructor(fun.symbol()));
		    constrBody.add(gen.Apply(superConstr, transform(args)));
		    break;
		default:
		    new scalac.ast.printer.TextTreePrinter().
			print(baseClasses[0]).println().end();
		    assert false;
		}
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
                            gen.Apply(
                                gen.mkRef(tree.pos, module_eq),
                                new Tree[] {gen.This(tree.pos, treeSym)}));
                    } else {
                        // !!! module_eq must be accessed via some outer field
                    }
                }
            }

	    // add valdefs and class-level expression to the constructorr body
	    constrBody.addAll(constrBody2);

	    // add result expression consistent with the
	    // result type of the constructor
            if (! forJVM)
                constrBody.add(gen.This(tree.pos, treeSym));
            Tree constrTree = constrBody.size() > 1 ?
                gen.Block((Tree[])constrBody.
			  toArray(new Tree[constrBody.size()])):
                (Tree) constrBody.get(0);

	    classBody.add(gen.DefDef(tree.pos, constrSym, constrTree));

	    // strip off the class constructor from parameters
	    switch (treeSym.primaryConstructor().info()) {
	    case MethodType(_, Type result):
		treeSym.primaryConstructor().
		    updateInfo(Type.MethodType(Symbol.EMPTY_ARRAY, result));
		break;
	    default : assert false;
	    }

	    Type classType =
		Type.compoundType(treeSym.parents(), classScope, treeSym);
	    Symbol classSym = treeSym.updateInfo(classType);
	    Tree[] newBody = (Tree[]) classBody.toArray(Tree.EMPTY_ARRAY);
	    return gen.ClassDef(classSym, baseClasses, newBody);

	// Substitute the constructor into the 'new' expressions
	case New(Template(Tree[] baseClasses, _)):
	    Tree base = baseClasses[0];
	    switch (base) {
	    case Apply(Tree fun, Tree[] args):
		//System.out.println(tree + " new " + fun.symbol());//DEBUG
                return gen.New(copy.Apply
			       (base,
				gen.Ident(base.pos, getConstructor(fun.symbol())),
				transform(args)));
	    default:
		assert false;
	    }
	    break;

	} // switch(tree)

	return super.transform(tree);
    } // transform()

} // class AddConstructors
